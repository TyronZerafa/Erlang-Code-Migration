-module(mcode_cb).

-import(resource_server,[add_local_resource/2,resource_available/1,get_resource/1]).
-import(evaluate,[load_function/1]).

-export([
			c_execute_remotely/1,
			c_execute_remotely/3,
			s_execute_remote_request/5,
			load_execute_fun/3
		]).

%------------------------------------------------------------
% Interface
%------------------------------------------------------------
c_execute_remotely(PF) ->
	expose_portable_function(PF),
	mcode:issue_remote_request(self()).
	
c_execute_remotely(M,F,A) ->
	Fun = fun M:F/A,
	PF = closure_rep:construct_full_fun_rep(Fun,[]),
	c_execute_remotely(PF).
	
s_execute_remote_request(RequestOrigin, RequestJobId, GLeader, Fun={M,F,_A}, Args) ->
	group_leader(GLeader,self()),
	reap_and_load_portable_function(Fun),
	mcode:remote_request_result(RequestOrigin, RequestJobId, {executing_remotely,self()}),
	mcode:remote_request_result(RequestOrigin, RequestJobId, apply(M,F,Args)).	
	
load_execute_fun(M,F,Args) ->
	Arity = length(Args),
	reap_and_load_portable_function({M,F,Arity}),
	case Arity of
		0 -> 
			M:F();
		_Else ->
			erlang:apply(M,F,Args)
	end.
	
%------------------------------------------------------------
% Helper functions
%------------------------------------------------------------
%% Exposes a portable function
expose_portable_function(PF) ->
	{module,Mod} = erlang:portable_fun_info(PF,module),
	{name,Name} = erlang:portable_fun_info(PF,name),
	{arity,Arity} = erlang:portable_fun_info(PF,arity),
	{code,Code} = erlang:portable_fun_info(PF,code),
	{calls,Calls} = erlang:portable_fun_info(PF,calls),
	expose({Mod,Name,Arity},Code,Calls),
	expose_portable_function_dependencies(PF,Calls).
	
expose_portable_function_dependencies(_PF,[]) -> [];
expose_portable_function_dependencies(PF,[H|T]) -> 
	{dependency_code,Code} = erlang:portable_fun_info(PF,{dependency_code,H}),
	{dependency_dep,Calls} = erlang:portable_fun_info(PF,{dependency_dep,H}),
	expose(H,Code,Calls),
	%% TODO: (T++Calls) possible infinite recursion
	expose_portable_function_dependencies(PF,T++Calls).
	
expose(Fun,Code,Calls) ->	
	resource_server:add_local_resource( io_lib:format("~p:Code",[Fun]), Code),
	resource_server:add_local_resource( io_lib:format("~p:Dependencies",[Fun]), Calls).
	
%% Reaps a portable function
reap_and_load_portable_function(Fun) ->
	[EvalMode] = dict:fetch(eval,mcode:get_rules()),
	reap_and_load_portable_function([Fun], EvalMode).
	
reap_and_load_portable_function([],_EvalMode) -> [];
reap_and_load_portable_function([Fun = {M,_F,_A}|T], EvalMode) ->
	
	Loaded = code:is_loaded(M),
	
	if  
		Loaded -> 
			reap_and_load_portable_function(T,EvalMode);
			
		true ->
			Code = reap_function(Fun, EvalMode),
			load_function(Code),
			
			if 
				EvalMode == eager ->
					[Dep] = reap_dependencies(Fun),
					reap_and_load_portable_function(Dep,EvalMode);
				EvalMode == lazy ->
					""
					%io:format("Loading Code lazily")
			end,
			
			reap_and_load_portable_function(T,EvalMode)
	end.

reap_function(Fun,EvalMode) ->
	Code = get_resource(io_lib:format("~p:Code",[Fun])),
	case EvalMode of
		lazy ->
			closure_rep:convert_to_lazy([Code]);
		_Else -> 
			lists:flatten(Code)
	end.
		
reap_dependencies(Fun) ->
	get_resource(io_lib:format("~p:Dependencies",[Fun])).	
