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
  , cancelActivity
  , heartbeatActivity
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

-- | Poll for activities.
--
pollActivity :: MonadAmazonWork c m => m (Maybe Text, Maybe Text, Maybe Text)
pollActivity = do
  d  <- view cDomain <$> view ccConf
  tl <- taskList <$> view awcQueue
  runResourceT $ runAmazonCtx $ do
    pfatrs <- send (pollForActivityTask d tl)
    pure
      ( pfatrs ^. pfatrsTaskToken
      , view weWorkflowId <$> pfatrs ^. pfatrsWorkflowExecution
      , pfatrs ^. pfatrsInput
      )

-- | Poll for decisions.
--
pollDecision :: MonadAmazonWork c m => m (Maybe Text, [HistoryEvent])
pollDecision = do
  d      <- view cDomain <$> view ccConf
  tl     <- taskList <$> view awcQueue
  runResourceT $ runAmazonCtx $ do
    pfdtrs <- paginate (pollForDecisionTask d tl) $$ consume
    pure
      ( join $ headMay $ view pfdtrsTaskToken <$> pfdtrs
      , reverse $ concatMap (view pfdtrsEvents) pfdtrs
      )

-- | Count activities.
--
countActivities :: MonadAmazonWork c m => m Int
countActivities = do
  d   <- view cDomain <$> view ccConf
  tl  <- taskList <$> view awcQueue
  runResourceT $ runAmazonCtx $ do
    ptc <- send (countPendingActivityTasks d tl)
    pure $ fromIntegral (ptc ^. ptcCount)

-- | Count decisions.
--
countDecisions :: MonadAmazonWork c m => m Int
countDecisions = do
  d   <- view cDomain <$> view ccConf
  tl  <- taskList <$> view awcQueue
  runResourceT $ runAmazonCtx $ do
    ptc <- send (countPendingDecisionTasks d tl)
    pure $ fromIntegral (ptc ^. ptcCount)

-- | Successful job completion.
--
completeActivity :: MonadConf c m => Text -> Maybe Text -> m ()
completeActivity token output =
  runResourceT $ runAmazonCtx $
    void $ send $ set ratcResult output $ respondActivityTaskCompleted token

-- | Job failure.
--
failActivity :: MonadConf c m => Text -> m ()
failActivity token =
  runResourceT $ runAmazonCtx $
    void $ send $ respondActivityTaskFailed token

-- | Cancel activity.
--
cancelActivity :: MonadConf c m => Text -> m ()
cancelActivity token =
  runResourceT $ runAmazonCtx $
    void $ send $ respondActivityTaskCanceled token

-- | Heartbeat activity.
--
heartbeatActivity :: MonadConf c m => Text -> m Bool
heartbeatActivity token =
  runResourceT $ runAmazonCtx $ do
    rathrs <- send $ recordActivityTaskHeartbeat token
    pure (rathrs ^. rathrsCancelRequested)

-- | Successful decision completion.
--
completeDecision :: MonadConf c m => Text -> Decision -> m ()
completeDecision token d =
  runResourceT $ runAmazonCtx $
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
