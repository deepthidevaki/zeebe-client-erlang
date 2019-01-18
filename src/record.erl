-module(record).

%% API exports
-export([get_version/1,
         get_bpmn_process_id/1,
         get_workflow_key/1,
         get_workflows/1
        ]).


get_workflow_key(Record) ->
    maps:get(workflowKey, Record).

get_key(Record) ->
    maps:get(key, Record).

get_bpmn_process_id(Record) ->
    maps:get(bpmnProcessId, Record).

get_version(Record) ->
    maps:get(version, Record).

get_workflows(Record) ->
    maps:get(workflows, Record).
