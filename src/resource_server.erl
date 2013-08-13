-module(resource_server).

-behaviour(gen_server).

%% gen_server specific functions
-export([
			init/1, 
			handle_call/3, 
			handle_cast/2, 
			handle_info/2,
        	terminate/2, 
			code_change/3
		]).
		
%% Resource Discovery API
-export([
         	start_link/0,
         	add_target_resource_type/1,
         	add_local_resource/2,
         	fetch_resources/1,
         	trade_resources/0,
         	get_resource/1,
         	resource_available/1
        ]).

-define(SERVER, ?MODULE).

-record(state, {target_resource_types,
                local_resource_tuples,
                found_resource_tuples}).

%------------------------------------------------------------
% Interface
%------------------------------------------------------------
init([]) ->
    {ok, #state{target_resource_types = [],
                local_resource_tuples = dict:new(),
                found_resource_tuples = dict:new()}}.
                
start_link() ->
	case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of 
	{ok, Pid}->
		io:format("Resource Discovery Service started [~p]. \n",[Pid]),
	{ok,Pid};
	  Other ->
		  {error, Other}
    end.

add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).

add_local_resource(Type, Resource) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Resource}}).

fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).

resource_available(Resource) ->
	gen_server:call(?SERVER, {available, Resource}).
	
get_resource(Type) ->
 	gen_server:call(?SERVER, {get_resource, Type},1000000).

trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

%------------------------------------------------------------
% Callback Functions
%------------------------------------------------------------
%% Fetch resource
handle_call({available, Resource}, _From, State) ->
	case dict:find(Resource, State#state.local_resource_tuples) of
		[] -> 
			case dict:find(Resource, State#state.found_resource_tuples) of
				[] -> {reply, false, State};
				_Else -> {reply, true, State}
			end;	
		_Else -> {reply, true, State}
	end;
	
handle_call({fetch_resources, Type}, _From, State) ->
    {reply, dict:find(Type, State#state.found_resource_tuples), State};
   
handle_call({get_resource, Type}, _From, State) ->
	Local = dict:find(Type, State#state.local_resource_tuples),
	Found = dict:find(Type, State#state.found_resource_tuples),
	if 
		(Local =/= error) and (Local =/= {ok,[]}) ->
			{ok,Result} = Local,
			{reply, Result, State};
		(Found =/= error) and (Found =/= {ok,[]}) ->
			{ok,Result} = Found,
			{reply, Result, State};
		true ->
			{ok,R} = g(nodes(),Type),
			if 
				R =/= [] ->
					[Result] = R,
					NewFound = add_resource(Type,Result,State#state.found_resource_tuples),
					{reply,R,State#state{found_resource_tuples=NewFound}};
				true ->
					{reply,[],State}
			end
	end;
	
handle_call({retrieve_resource, Type}, _From, State) ->
	Local = dict:find(Type, State#state.local_resource_tuples),
	Found = dict:find(Type, State#state.found_resource_tuples),
	if
		(Local =/= error) and (Local =/= {ok,[]}) ->
			{ok,Result} = Local,
			{reply, Result, State};
		(Found =/= error) and (Found =/= {ok,[]}) ->
			{ok,Result} = Found,
			{reply, Result, State};
		true ->
			{reply, [], State}
	end.

g([],_Type) -> {ok,[]};
g([Node|T],Type) ->
	case gen_server:call({?SERVER, Node},{retrieve_resource,Type}) of
		error ->
			g(T,Type);
		[] ->
			g(T,Type);
		_Else ->
			{ok,_Else}
	end.
	
%% Add required resource
handle_cast({add_target_resource_type, Type}, State) ->
    TargetTypes = State#state.target_resource_types,
    NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
    {noreply, State#state{target_resource_types = NewTargetTypes}};

%%  Add local resources
handle_cast({add_local_resource, {Type, Resource}}, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    NewResourceTuples = add_resource(Type, Resource, ResourceTuples),
    {noreply, State#state{local_resource_tuples = NewResourceTuples}};

%% Trade local and remote resources
handle_cast(trade_resources, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    AllNodes = [node() | nodes()],
    lists:foreach(
        fun(Node) ->
            gen_server:cast({?SERVER, Node},
                            {trade_resources, {node(), ResourceTuples}})
        end,
        AllNodes),
    {noreply, State};

handle_cast({trade_resources, {ReplyTo, Remotes}},#state{local_resource_tuples = Locals,target_resource_types = TargetTypes,found_resource_tuples = OldFound} = State) ->
		  
    FilteredRemotes = resources_for_types(TargetTypes, Remotes),
    NewFound = add_resources(FilteredRemotes, OldFound),
    case ReplyTo of
        noreply ->
            ok;
        _ ->
            gen_server:cast({?SERVER, ReplyTo},
                            {trade_resources, {noreply, Locals}})
    end,
    {noreply, State#state{found_resource_tuples = NewFound}}.

%% Handle gen_server info
handle_info(ok = _Info, State) ->
    {noreply, State}.

%% Handle gen_server terminate
terminate(_Reason, _State) ->
	io:format("Terminating resource discovery service [~s]", [_Reason]).

%% Handle gen_server version update 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%------------------------------------------------------------
% Helper Functions - Utilities
%------------------------------------------------------------
add_resources([{Type, Identifier}|T], Dict) ->
    add_resources(T, add_resource(Type, Identifier, Dict));
add_resources([], Dict) ->
    Dict.

add_resource(Type, Resource, Dict) ->
    case dict:find(Type, Dict) of
        {ok, ResourceList} ->
            NewList = [Resource | lists:delete(Resource, ResourceList)],
            dict:store(Type, NewList, Dict);
        error ->
            dict:store(Type, [Resource], Dict)
    end.
	
resources_for_types(Types, ResourceTuples) ->
    Fun =
        fun(Type, Acc) ->
            case dict:find(Type, ResourceTuples) of
                {ok, List} ->
                    [{Type, Resource} || Resource <- List] ++ Acc;
                error ->
                    Acc
            end
        end,
    lists:foldl(Fun, [], Types).
