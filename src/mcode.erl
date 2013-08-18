-module(mcode).

-behaviour(gen_server).
		
%% gen_server Specific Functions
-export([
			init/1,
			start_link/0,
			handle_cast/2,
			handle_call/3,
			handle_info/2,
    	    terminate/2, 
			code_change/3
		]).

%% Remote Evaluation Specific Functions
-export([
			remote_spawn/4,
			remote_spawn/6,
			issue_remote_request/1,
			remote_request_result/3,
			get_result/1,
			get_rules/0,
			reload_rules/0
        ]).

-define(SERVER, ?MODULE).

-record(state, {rules, running_tasks}).
-record(job, {pid, state, requestNode, funSpec, groupLeader, arguments}).
                
%------------------------------------------------------------
% Interface
%------------------------------------------------------------
init([]) -> 
	process_flag(trap_exit, true),
	Rules = pf_rules_parser:compile_rules_from_file("./policy_file.conf"),
	io:format("~p ~n",[Rules]),
	{ok, #state{rules = Rules, running_tasks = dict:new()}}.
	
start_link() ->
	case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of 
		{ok, Pid}->
			io:format("Remote Evaluation Service started [~p]. \n",[Pid]),
			{ok,Pid};
	  	Other ->
		 	{error, Other}
    	end.

% Remotely execute an anonymous portable function on a remote node
remote_spawn(Node, PortableFunction, GLeader, Timeout) ->
	gen_server:call(?SERVER, {c_remote_execute, {Node, PortableFunction, GLeader}}, Timeout).

% Remotely execute a named portabled function on a remote node
remote_spawn(Node, M, F, Args, GLeader, Timeout) ->
	gen_server:call(?SERVER, {c_remote_execute, {Node, M, F, Args, GLeader}},Timeout).
		
% Send a remote evaluation request to a node
issue_remote_request(JobId) ->
	gen_server:call(?SERVER, {c_remote_execute, JobId}).

% Send the result of a remote evaluation request
remote_request_result(NodeOrigin, OriginJobId, Result) ->
	gen_server:cast({?SERVER, NodeOrigin}, {c_result_update, OriginJobId, Result}).
	
% Returns the result of a remotely executed function
get_result(JobId) when erlang:is_pid(JobId) ->
	gen_server:call(?SERVER, {get_result, JobId});
get_result(JobId) ->
	throw(io:format("\"~p\" is not a valid JobId ~n",[JobId])).

% Returns the policy file rules	
get_rules() ->
	gen_server:call(?SERVER, {get_rules}).
	
% Reloads the policy rules from file
reload_rules() ->
	gen_server:cast({?SERVER, node()}, {reload_rules}).
		
%------------------------------------------------------------
% Callback Functions
%------------------------------------------------------------
%% --- Handle Calls ---
handle_call({c_remote_execute,{Node, PortableFunction, GLeader}}, _From, State) ->
	Pid = spawn_link(mcode_cb, c_execute_remotely, [PortableFunction]),
	{module,M} = erlang:portable_fun_info(PortableFunction,module),
	{name,F} = erlang:portable_fun_info(PortableFunction,name),
	{arity,A} = erlang:portable_fun_info(PortableFunction,arity),
	Job = #job{pid=Pid, state=exposing_code, requestNode=Node, funSpec={M,F,A}, groupLeader=GLeader, arguments=[]},
	NewState = dict:store(pid_to_list(Pid), Job, State#state.running_tasks),
	{reply, Job#job.pid, State#state{running_tasks = NewState}};

handle_call({c_remote_execute,{Node, M, F, Arguments, GLeader}}, _From, State) ->
	A = erlang:length(Arguments),
	Pid = spawn_link(mcode_cb, c_execute_remotely, [M,F,A]),
	NewMod = list_to_atom(lists:flatten(io_lib:format("~s-~s@~s",[M,F,node()]))),
	Job = #job{pid=Pid, state=exposing_code, requestNode=Node, funSpec={NewMod,F,A}, groupLeader=GLeader, arguments=Arguments},
	NewState = dict:store(pid_to_list(Pid), Job, State#state.running_tasks),
	{reply, Job#job.pid, State#state{running_tasks = NewState}};
	
handle_call({c_remote_execute, JobId}, _From, State) ->
	timer:sleep(10),
	Job = dict:fetch(pid_to_list(JobId), State#state.running_tasks),
	gen_server:cast({?MODULE, Job#job.requestNode}, {s_remote_execute, {node(), JobId, Job#job.groupLeader, Job#job.funSpec, Job#job.arguments}}),
	NewJob = Job#job{state=requesting_execution},
	NewState = dict:store(pid_to_list(JobId), NewJob, State#state.running_tasks),
	{reply, [], State#state{running_tasks = NewState}};

handle_call({get_result, JobId}, _From, State) ->
	Job = dict:fetch(pid_to_list(JobId), State#state.running_tasks),
	{reply, Job#job.state, State};
	
handle_call({get_rules}, _From, State) ->
	{reply, State#state.rules, State}.
	
%% --- Handle Casts ---	
handle_cast({s_remote_execute, {RequestNodeOrigin, RequestJobId, GLeader, Fun, Args}}, State) ->
	spawn_link(mcode_cb,s_execute_remote_request, [RequestNodeOrigin, RequestJobId, GLeader, Fun, Args]),	
	{noreply, State};

handle_cast({c_result_update, JobId, Result}, State) ->
	Job = dict:fetch(pid_to_list(JobId), State#state.running_tasks),
	NewJob = Job#job{state={result,Result}},
	NewState = dict:store(pid_to_list(JobId), NewJob, State#state.running_tasks),
	{noreply, State#state{running_tasks = NewState}};
	
handle_cast({reload_rules}, State) ->
	Rules = pf_rules_parser:compile_rules_from_file("./policy_file.conf"),
	{noreply, State#state{rules = Rules}};
	
handle_cast(Anything, State) ->
	io:fwrite("Unknown non blocking call received: [~p]. \n", [Anything]),
	{noreply, State}.

%% -- Handle Info --
handle_info({get_server_state}, State) ->
	io:fwrite("Server state: ~p. \n", [dict:to_list(State#state.running_tasks)]),
	{noreply, State};

handle_info({'EXIT',_Pid,normal},State) ->
	{noreply, State};

handle_info({'EXIT',Pid,_Reason},State) ->
	io:fwrite("Child process [~p] died. \n",[{Pid}]),
	{noreply, State};

handle_info(A, State) ->
	io:fwrite("Received info: [~p]. \n",[A]),
	{noreply, State}.

%% Terminate
terminate(Reason, _State) ->
	io:fwrite("Terminated reason: [~p]. \n", [Reason]),
    	ok.

%% Code update
code_change(_OldVsn, State, _Extra) ->
    	{ok, State}.
