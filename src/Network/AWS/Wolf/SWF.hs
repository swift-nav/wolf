{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | SWF Calls.
--
module Network.AWS.Wolf.SWF
  ( pollActivity
  , pollDecision
  , completeActivity
  , failActivity
  , completeDecision
  , scheduleWork
  , completeWork
  , failWork
  ) where

import Control.Monad.Trans.AWS
import Data.Conduit
import Data.Conduit.List        hiding (concatMap, map)
import Network.AWS.SWF
import Network.AWS.Wolf.Prelude
import Network.AWS.Wolf.Types

-- | Poll for activities.
--
pollActivity :: MonadAmazonWork c m => m (Maybe Text, Maybe Text, Maybe Text)
pollActivity = do
  d      <- view cDomain <$> view ccConf
  tl     <- taskList <$> view awcQueue
  pfatrs <- send (pollForActivityTask d tl)
  return
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
  pfdtrs <- paginate (pollForDecisionTask d tl) $$ consume
  return
    ( join $ headMay $ map (view pfdtrsTaskToken) pfdtrs
    , reverse $ concatMap (view pfdtrsEvents) pfdtrs
    )

-- | Successful job completion.
--
completeActivity :: MonadAmazon c m => Text -> Maybe Text -> m ()
completeActivity token output =
  void $ send $ set ratcResult output $ respondActivityTaskCompleted token

-- | Job failure.
--
failActivity :: MonadAmazon c m => Text -> m ()
failActivity token =
  void $ send $ respondActivityTaskFailed token

-- | Successful decision completion.
--
completeDecision :: MonadAmazon c m => Text -> Decision -> m ()
completeDecision token d =
  void $ send $ set rdtcDecisions (return d) $ respondDecisionTaskCompleted token

-- | Schedule decision.
--
scheduleWork :: Text -> Text -> Text -> Text -> Maybe Text -> Decision
scheduleWork uid name version queue input =
  decision ScheduleActivityTask &
    dScheduleActivityTaskDecisionAttributes .~ return satda
  where
    satda =
      scheduleActivityTaskDecisionAttributes (activityType name version) uid &
        satdaTaskList .~ return (taskList queue) &
        satdaInput .~ input

-- | Complete decision.
--
completeWork :: Maybe Text -> Decision
completeWork input =
  decision CompleteWorkflowExecution &
    dCompleteWorkflowExecutionDecisionAttributes .~ return cweda
  where
    cweda =
      completeWorkflowExecutionDecisionAttributes &
        cwedaResult .~ input

-- | Failed decision.
--
failWork :: Decision
failWork =
  decision FailWorkflowExecution &
    dFailWorkflowExecutionDecisionAttributes .~ return fweda
  where
    fweda =
      failWorkflowExecutionDecisionAttributes


