{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}

module Network.AWS.Flow.SWF
  ( registerDomainAction
  , registerActivityTypeAction
  , registerWorkflowTypeAction
  , startWorkflowExecutionAction
  , pollForActivityTaskAction
  , respondActivityTaskCompletedAction
  , respondActivityTaskFailedAction
  , pollForDecisionTaskAction
  , respondDecisionTaskCompletedAction
  , scheduleActivityTaskDecision
  , completeWorkflowExecutionDecision
  , startTimerDecision
  , continueAsNewWorkflowExecutionDecision
  ) where

import Control.Lens              ( (^.), (.~), (&) )
import Control.Monad             ( liftM )
import Control.Monad.Reader      ( asks )
import Control.Monad.Trans.AWS   ( paginate, send, send_ )
import Data.Conduit              ( ($$) )
import Data.Conduit.List         ( consume )
import Network.AWS.Flow.Types
import Network.AWS.Flow.Internal ( runAWS )
import Network.AWS.SWF
import Safe                      ( headMay )

-- Actions

registerDomainAction :: MonadFlow m => m ()
registerDomainAction = do
  domain <- asks feDomain
  runAWS feEnv $
    send_ $ registerDomain domain "30"

registerActivityTypeAction :: MonadFlow m => Name -> Version -> Timeout -> m ()
registerActivityTypeAction name version timeout = do
  domain <- asks feDomain
  runAWS feEnv $
    send_ $ registerActivityType domain name version &
      ratDefaultTaskHeartbeatTimeout .~ Just "NONE" &
      ratDefaultTaskScheduleToCloseTimeout .~ Just "NONE" &
      ratDefaultTaskScheduleToStartTimeout .~ Just "60" &
      ratDefaultTaskStartToCloseTimeout .~ Just timeout

registerWorkflowTypeAction :: MonadFlow m => Name -> Version -> Timeout -> m ()
registerWorkflowTypeAction name version timeout = do
  domain <- asks feDomain
  runAWS feEnv $
    send_ $ registerWorkflowType domain name version &
      rwtDefaultChildPolicy .~ Just Terminate &
      rwtDefaultExecutionStartToCloseTimeout .~ Just timeout &
      rwtDefaultTaskStartToCloseTimeout .~ Just "60"

startWorkflowExecutionAction :: MonadFlow m
                             => Uid -> Name -> Version -> Queue -> Metadata -> m ()
startWorkflowExecutionAction uid name version queue input = do
  domain <- asks feDomain
  runAWS feEnv $
    send_ $ startWorkflowExecution domain uid (workflowType name version) &
      swe1TaskList .~ Just (taskList queue) &
      swe1Input .~ input

pollForActivityTaskAction :: MonadFlow m => Queue -> m (Token, Metadata)
pollForActivityTaskAction queue = do
  domain <- asks feDomain
  runAWS fePollEnv $ do
    r <- send $ pollForActivityTask domain (taskList queue)
    return
      ( r ^. pfatrTaskToken
      , r ^. pfatrInput )

respondActivityTaskCompletedAction :: MonadFlow m => Token -> Metadata -> m ()
respondActivityTaskCompletedAction token result =
  runAWS feEnv $
    send_ $ respondActivityTaskCompleted token &
      ratcResult .~ result

respondActivityTaskFailedAction :: MonadFlow m => Token -> m ()
respondActivityTaskFailedAction token =
  runAWS feEnv $
    send_ $ respondActivityTaskFailed token

pollForDecisionTaskAction :: MonadFlow m => Queue -> m (Maybe Token, [HistoryEvent])
pollForDecisionTaskAction queue = do
  domain <- asks feDomain
  runAWS fePollEnv $ do
    rs <- paginate (pollForDecisionTask domain (taskList queue) &
      pfdtReverseOrder .~ Just True &
      pfdtMaximumPageSize .~ Just 100)
        $$ consume
    return
      ( liftM (^. pfdtrTaskToken) (headMay rs)
      , concatMap (^. pfdtrEvents) rs)

respondDecisionTaskCompletedAction :: MonadFlow m => Token -> [Decision] -> m ()
respondDecisionTaskCompletedAction token decisions =
  runAWS feEnv $
    send_ $ respondDecisionTaskCompleted token &
      rdtcDecisions .~ decisions

-- Decisions

scheduleActivityTaskDecision :: Uid -> Name -> Version -> Queue -> Metadata -> Decision
scheduleActivityTaskDecision uid name version list input =
  decision ScheduleActivityTask &
    dScheduleActivityTaskDecisionAttributes .~ Just attrs where
      attrs = scheduleActivityTaskDecisionAttributes (activityType name version) uid &
        satdaTaskList .~ Just (taskList list) &
        satdaInput .~ input

completeWorkflowExecutionDecision :: Metadata -> Decision
completeWorkflowExecutionDecision result =
  decision CompleteWorkflowExecution &
    dCompleteWorkflowExecutionDecisionAttributes .~ Just attrs where
      attrs = completeWorkflowExecutionDecisionAttributes &
        cwedaResult .~ result

startTimerDecision :: Uid -> Name -> Timeout -> Decision
startTimerDecision uid name timeout =
  decision StartTimer &
    dStartTimerDecisionAttributes .~ Just attrs where
      attrs = startTimerDecisionAttributes uid timeout &
        stdaControl .~ Just name

continueAsNewWorkflowExecutionDecision :: Version -> Queue -> Metadata -> Decision
continueAsNewWorkflowExecutionDecision version queue input =
  decision ContinueAsNewWorkflowExecution &
    dContinueAsNewWorkflowExecutionDecisionAttributes .~ Just attrs where
      attrs = continueAsNewWorkflowExecutionDecisionAttributes &
        canwedaWorkflowTypeVersion .~ Just version &
        canwedaTaskList .~ Just (taskList queue) &
        canwedaInput .~ input
