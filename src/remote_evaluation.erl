-module(remote_evaluation).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
	start([],[]).
	
start(_StartType, _StartArgs) ->
    case mcode_supervisor:start_link() of
        {ok, Pid} ->
			io:format("Rev App started with PID: ~p",[Pid]),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
