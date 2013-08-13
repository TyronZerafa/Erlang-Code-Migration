-module(mcode_supervisor).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([start_link/0, init/1]).

start_link() ->
	{ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
	io:format("Remote Evaluation Supervisor started [~p] ~n",[Pid]),
	{ok,Pid}.

init([]) ->
	RemoteEvaluationServer = {mcode, {mcode, start_link, []},
              permanent, 2000, worker, [rev]},

	ResourceDiscoveryServer = {resource_server, {resource_server, start_link, []},
              permanent, 2000, worker, [resource_server]},

	Children = [ ResourceDiscoveryServer,RemoteEvaluationServer],
    
    RestartStrategy = {one_for_one, 2, 2000},
    
    {ok, {RestartStrategy, Children}}.	
