-module(zbclient).

%% API exports
-export([create_workflow_instance/2,
         create_workflow_instance/4,
         deploy_workflow/3,
         deploy_workflows/1,
         get_version/1,
         get_bpmn_process_id/1,
         get_workflow_key/1,
         get_workflows/1
        ]).



%%====================================================================
%% API functions
%%====================================================================

create_workflow_instance(WorkflowKey, BpmnProcessId, Version, Payload) ->
  Request =   #{workflowKey => WorkflowKey,
                bpmnProcessId => BpmnProcessId,
                version => Version,
                payload => Payload},
  Response = gateway_protocol_gateway_client:create_workflow_instance(ctx:new(), Request),
  case Response of
    {ok, Result,_Metadata} ->
            Result;
    Error -> Error
  end.

create_workflow_instance(WorkflowKey, BpmnProcessId) ->
    create_workflow_instance(WorkflowKey, BpmnProcessId, -1, "").

deploy_workflow(Name, Type, FilePath) ->
    deploy_workflows([{Name, Type, FilePath}]).

deploy_workflows(Workflows) ->
    WorkflowObjects = lists:map(
                        fun({Name, Type, FilePath}) ->
                            {ok, Object} = get_workflow_object(Name, Type, FilePath),
                            Object
                        end, Workflows),
    Request = #{workflows => WorkflowObjects},
    Response = gateway_protocol_gateway_client:deploy_workflow(ctx:new(), Request),
    case Response of
        {ok, WorkflowMetadata, _GrpcMetadata} -> WorkflowMetadata;
        Error -> Error
    end.

get_workflow_key(#{workflowKey := Value}) ->
    {ok, Value}.

get_bpmn_process_id(#{bpmnProcessId := Value}) ->
    {ok, Value}.

get_version(#{version := Value}) ->
        {ok, Value}.

get_workflows(#{workflows := Value}) ->
        {ok, Value}.

%%====================================================================
%% Internal functions
%%====================================================================

get_workflow_object(Name, Type, FilePath) ->
    case file:read_file(FilePath) of
        {ok, Resource} ->
            {ok, #{name => Name, type => Type, definition => Resource}};
        {error, Reason} ->
            {error_reading_file, Reason}
    end.
