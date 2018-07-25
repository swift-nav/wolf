{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Calls.
--
module Network.AWS.Wolf.SWF
  ( pollActivity
  , pollDecision
  , countActivities
  , countDecisions
  , completeActivity
  , failActivity
  , completeDecision
  , scheduleWork
  , completeWork
  , failWork
  ) where

import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Conduit.List        hiding (concatMap)
import Network.AWS.SWF
import Network.AWS.Wolf.Ctx
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types
import Control.Monad.IO.Unlift

-- | Poll for activities.
--
pollActivity :: (MonadBaseControl IO m, MonadResource m, MonadAmazonWork c m) => m (Maybe Text, Maybe Text, Maybe Text)
pollActivity = do
  d  <- view cDomain <$> view ccConf
  tl <- taskList <$> view awcQueue
  runAmazonCtx $ do
    pfatrs <- send (pollForActivityTask d tl)
    pure
      ( pfatrs ^. pfatrsTaskToken
      , view weWorkflowId <$> pfatrs ^. pfatrsWorkflowExecution
      , pfatrs ^. pfatrsInput
      )

-- | Poll for decisions.
--
pollDecision :: (MonadBaseControl IO m, MonadResource m, MonadAmazonWork c m) => m (Maybe Text, [HistoryEvent])
pollDecision = do
  d      <- view cDomain <$> view ccConf
  tl     <- taskList <$> view awcQueue
  runAmazonCtx $ do
    pfdtrs <- paginate (pollForDecisionTask d tl) $$ consume
    pure
      ( join $ headMay $ view pfdtrsTaskToken <$> pfdtrs
      , reverse $ concatMap (view pfdtrsEvents) pfdtrs
      )

-- | Count activities.
--
countActivities :: (MonadBaseControl IO m, MonadResource m, MonadAmazonWork c m) => m Int
countActivities = do
  d   <- view cDomain <$> view ccConf
  tl  <- taskList <$> view awcQueue
  runAmazonCtx $ do
    ptc <- send (countPendingActivityTasks d tl)
    pure $ fromIntegral (ptc ^. ptcCount)

-- | Count decisions.
--
countDecisions :: (MonadBaseControl IO m, MonadResource m, MonadAmazonWork c m) => m Int
countDecisions = do
  d   <- view cDomain <$> view ccConf
  tl  <- taskList <$> view awcQueue
  runAmazonCtx $ do
    ptc <- send (countPendingDecisionTasks d tl)
    pure $ fromIntegral (ptc ^. ptcCount)

-- | Successful job completion.
--
completeActivity :: (MonadBaseControl IO m, MonadResource m, MonadConf c m) => Text -> Maybe Text -> m ()
completeActivity token output =
  runAmazonCtx $
    void $ send $ set ratcResult output $ respondActivityTaskCompleted token

-- | Job failure.
--
failActivity :: (MonadBaseControl IO m, MonadResource m, MonadConf c m) => Text -> m ()
failActivity token =
  runAmazonCtx $
    void $ send $ respondActivityTaskFailed token

-- | Successful decision completion.
--
completeDecision :: (MonadBaseControl IO m, MonadResource m, MonadConf c m) => Text -> Decision -> m ()
completeDecision token d =
  runAmazonCtx $
    void $ send $ set rdtcDecisions (pure d) $ respondDecisionTaskCompleted token

-- | Schedule decision.
--
scheduleWork :: Text -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Decision
scheduleWork uid name version queue input priority =
  decision ScheduleActivityTask &
    dScheduleActivityTaskDecisionAttributes .~ pure satda
  where
    satda =
      scheduleActivityTaskDecisionAttributes (activityType name version) uid &
        satdaTaskList .~ pure (taskList queue) &
        satdaInput .~ input &
        satdaTaskPriority .~ priority

-- | Complete decision.
--
completeWork :: Maybe Text -> Decision
completeWork input =
  decision CompleteWorkflowExecution &
    dCompleteWorkflowExecutionDecisionAttributes .~ pure cweda
  where
    cweda =
      completeWorkflowExecutionDecisionAttributes &
        cwedaResult .~ input

-- | Failed decision.
--
failWork :: Decision
failWork =
  decision FailWorkflowExecution &
    dFailWorkflowExecutionDecisionAttributes .~ pure fweda
  where
    fweda =
      failWorkflowExecutionDecisionAttributes
