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
  , startChildWorkflowExecutionDecision
  ) where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.AWS hiding ( Metadata )
import Data.Conduit
import Data.Conduit.List hiding ( concatMap )
import Network.AWS.Flow.Types
import Network.AWS.Flow.Internal
import Network.AWS.SWF
import Safe

-- Actions

registerDomainAction :: MonadFlow m => m ()
registerDomainAction = do
  domain <- asks feDomain
  runAWS feTimeout $ void $
    send $ registerDomain domain "30"

registerActivityTypeAction :: MonadFlow m => Name -> Version -> Timeout -> m ()
registerActivityTypeAction name version t = do
  domain <- asks feDomain
  runAWS feTimeout $ void $
    send $ registerActivityType domain name version &
      ratDefaultTaskHeartbeatTimeout .~ Just "NONE" &
      ratDefaultTaskScheduleToCloseTimeout .~ Just "NONE" &
      ratDefaultTaskScheduleToStartTimeout .~ Just "60" &
      ratDefaultTaskStartToCloseTimeout .~ Just t

registerWorkflowTypeAction :: MonadFlow m => Name -> Version -> Timeout -> m ()
registerWorkflowTypeAction name version t = do
  domain <- asks feDomain
  runAWS feTimeout $ void $
    send $ registerWorkflowType domain name version &
      rwtDefaultChildPolicy .~ Just Abandon &
      rwtDefaultExecutionStartToCloseTimeout .~ Just t &
      rwtDefaultTaskStartToCloseTimeout .~ Just "60"

startWorkflowExecutionAction :: MonadFlow m
                             => Uid -> Name -> Version -> Queue -> Metadata -> m ()
startWorkflowExecutionAction uid name version queue input = do
  domain <- asks feDomain
  runAWS feTimeout $ void $
    send $ startWorkflowExecution domain uid (workflowType name version) &
      sTaskList .~ Just (taskList queue) &
      sInput .~ input

pollForActivityTaskAction :: MonadFlow m => Queue -> m (Token, Uid, Metadata)
pollForActivityTaskAction queue = do
  domain <- asks feDomain
  runAWS fePollTimeout $ do
    r <- send $ pollForActivityTask domain (taskList queue)
    return
      ( r ^. pfatrsTaskToken
      , r ^. pfatrsWorkflowExecution ^. weWorkflowId
      , r ^. pfatrsInput )

respondActivityTaskCompletedAction :: MonadFlow m => Token -> Metadata -> m ()
respondActivityTaskCompletedAction token result =
  runAWS feTimeout $ void $
    send $ respondActivityTaskCompleted token &
      ratcResult .~ result

respondActivityTaskFailedAction :: MonadFlow m => Token -> m ()
respondActivityTaskFailedAction token =
  runAWS feTimeout $ void $
    send $ respondActivityTaskFailed token

pollForDecisionTaskAction :: MonadFlow m => Queue -> m (Maybe Token, [HistoryEvent])
pollForDecisionTaskAction queue = do
  domain <- asks feDomain
  runAWS fePollTimeout $ do
    rs <- paginate (pollForDecisionTask domain (taskList queue) &
      pfdtReverseOrder .~ Just True &
      pfdtMaximumPageSize .~ Just 100)
        $$ consume
    return
      ( liftM (^. pfdtrsTaskToken) (headMay rs)
      , concatMap (^. pfdtrsEvents) rs)

respondDecisionTaskCompletedAction :: MonadFlow m => Token -> [Decision] -> m ()
respondDecisionTaskCompletedAction token decisions =
  runAWS feTimeout $ void $
    send $ respondDecisionTaskCompleted token &
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
startTimerDecision uid name t =
  decision StartTimer &
    dStartTimerDecisionAttributes .~ Just attrs where
      attrs = startTimerDecisionAttributes uid t &
        stdaControl .~ Just name

continueAsNewWorkflowExecutionDecision :: Version -> Queue -> Metadata -> Decision
continueAsNewWorkflowExecutionDecision version queue input =
  decision ContinueAsNewWorkflowExecution &
    dContinueAsNewWorkflowExecutionDecisionAttributes .~ Just attrs where
      attrs = continueAsNewWorkflowExecutionDecisionAttributes &
        canwedaWorkflowTypeVersion .~ Just version &
        canwedaTaskList .~ Just (taskList queue) &
        canwedaInput .~ input

startChildWorkflowExecutionDecision :: Uid -> Name -> Version -> Queue -> Metadata -> Decision
startChildWorkflowExecutionDecision uid name version queue input =
  decision StartChildWorkflowExecution &
    dStartChildWorkflowExecutionDecisionAttributes .~ Just attrs where
      attrs = startChildWorkflowExecutionDecisionAttributes (workflowType name version) uid &
        scwedaTaskList .~ Just (taskList queue) &
        scwedaInput .~ input
