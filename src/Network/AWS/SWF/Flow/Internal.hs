{-# LANGUAGE RecordWildCards #-}

module Network.AWS.SWF.Flow.Internal where

import Control.Lens               ( (^.), (.~), (&), (<&>) )
import Control.Monad              ( liftM )
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Except ( runExceptT )
import Data.Conduit               ( ($$) )
import Data.Conduit.List          ( consume )
import Data.Maybe                 ( listToMaybe )
import Data.Text                  ( Text, pack )
import Data.UUID                  ( toString )
import Data.UUID.V4               ( nextRandom )
import Network.AWS.SWF
import Network.AWS.SWF.Flow.Types
import Network.HTTP.Conduit       ( conduitManagerSettings )
import Network.HTTP.Client        ( ManagerSettings(..), withManager )

-- Helpers

withContext :: Config -> (Context -> IO a) -> IO a
withContext Config {..} action =
  let
    managerSettings timeout =
      conduitManagerSettings { managerResponseTimeout = Just timeout }

    newEnv' region credentials manager =
      runExceptT (newEnv region credentials manager) >>= either error return
  in
    withManager (managerSettings cfgTimeout) $ \manager -> do
      withManager (managerSettings cfgPollTimeout) $ \pollManager -> do
        uid <- liftM (pack . toString) nextRandom
        logger <- newLogger cfgLogLevel cfgLogHandle
        env <- newEnv' cfgRegion cfgCredentials manager <&> envLogger .~ logger
        pollEnv <- newEnv' cfgRegion cfgCredentials pollManager <&> envLogger .~ logger
        action $ Context uid env pollEnv

-- Runners

runStartWorkflowExecution :: Env -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> IO (EitherE ())
runStartWorkflowExecution env domain uid name version list input =
  runAWST env $
    send_ $ startWorkflowExecution  domain uid (workflowType name version) &
      swe1TaskList .~ Just (taskList list) &
      swe1Input .~ input

runPollForActivityTask :: Env -> Text -> Text -> Text -> IO (EitherE (Text, Maybe Text))
runPollForActivityTask env domain uid list =
  runAWST env $ do
    r <- send $ pollForActivityTask domain (taskList list) &
      pfatIdentity .~ Just uid
    return $
      ( r ^. pfatrTaskToken
      , r ^. pfatrInput )

runRespondActivityTaskCompleted :: Env -> Text -> Maybe Text -> IO (EitherE ())
runRespondActivityTaskCompleted env taskToken result =
  runAWST env $
    send_ $ respondActivityTaskCompleted taskToken &
      ratcResult .~ result

runPollForDecisionTask :: Env -> Text -> Text -> Text -> IO (EitherE (Maybe Text, [HistoryEvent]))
runPollForDecisionTask env domain uid list =
  runAWST env $ do
    rs <- paginate ( pollForDecisionTask domain (taskList list) &
      pfdtIdentity .~ Just uid &
      pfdtReverseOrder .~ Just True &
      pfdtMaximumPageSize .~ Just 100 )
        $$ consume
    return $
      ( listToMaybe rs >>= return . (^. pfdtrTaskToken)
      , concatMap (^. pfdtrEvents) rs)

runRespondDecisionTaskCompleted :: Env -> Text -> [Decision] -> IO (EitherE ())
runRespondDecisionTaskCompleted env taskToken decisions =
  runAWST env $
    send_ $ respondDecisionTaskCompleted taskToken &
      rdtcDecisions .~ decisions

-- Decisions

scheduleActivityTask :: Text -> Text -> Text -> Text -> Maybe Text -> Decision
scheduleActivityTask uid name version list input =
  decision ScheduleActivityTask &
    dScheduleActivityTaskDecisionAttributes .~ Just attrs where
      attrs = scheduleActivityTaskDecisionAttributes (activityType name version) uid &
        satdaTaskList .~ Just (taskList list) &
        satdaInput .~ input

completeWorkflowExecution :: Maybe Text -> Decision
completeWorkflowExecution result =
  decision CompleteWorkflowExecution &
    dCompleteWorkflowExecutionDecisionAttributes .~ Just attrs where
      attrs = completeWorkflowExecutionDecisionAttributes &
        cwedaResult .~ result

startTimer :: Text -> Text -> Text -> Decision
startTimer uid timeout name =
  decision StartTimer &
    dStartTimerDecisionAttributes .~ Just attrs where
      attrs = startTimerDecisionAttributes uid timeout &
        stdaControl .~ Just name

continueAsNewWorkflowExecution :: Text -> Text -> Maybe Text -> Decision
continueAsNewWorkflowExecution version list input =
  decision ContinueAsNewWorkflowExecution &
    dContinueAsNewWorkflowExecutionDecisionAttributes .~ Just attrs where
      attrs = continueAsNewWorkflowExecutionDecisionAttributes &
        canwedaWorkflowTypeVersion .~ Just version &
        canwedaTaskList .~ Just (taskList list) &
        canwedaInput .~ input
