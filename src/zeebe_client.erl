-module(zeebe_client).

%% API exports
-export([create_workflow_instance/2,
         create_workflow_instance/4,
         deploy_workflow/3,
         deploy_workflows/1,
         activate_jobs/4,
         cancel_job_handler/1
        ]).



%%====================================================================
%% API functions
%%====================================================================

create_workflow_instance(WorkflowKey, BpmnProcessId, Version, Payload) ->
    workflow:create_workflow_instance(WorkflowKey, BpmnProcessId, Version, Payload).

create_workflow_instance(WorkflowKey, BpmnProcessId) ->
    create_workflow_instance(WorkflowKey, BpmnProcessId, -1, "").

deploy_workflow(Name, Type, FilePath) ->
    deploy_workflows([{Name, Type, FilePath}]).

deploy_workflows(Workflows) ->
    workflow:deploy_workflows(Workflows).

activate_jobs(Type, Worker, Timeout, MaxJobsToActivate) ->
    job_handler:activate_jobs(Type, Worker, Timeout, MaxJobsToActivate).

cancel_job_handler(Pid) ->
    gen_statem:stop(Pid).


%list_workflows(BpmnProcessId) ->
%    workflow:list_workflows(BpmnProcessId).

%%====================================================================
%% Internal functions
%%====================================================================
