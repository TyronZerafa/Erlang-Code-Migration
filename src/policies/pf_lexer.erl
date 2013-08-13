%% ************************************************************
%% Policy files lexer - parses policy file tokens
%% ************************************************************
-module(pf_lexer).

-export([file/1,string/1]).

%------------------------------------------------------------
% Interface
%------------------------------------------------------------
file(Filename) ->
	{ok, Binary} = file:read_file(Filename),
	file:close(Filename),
	String = erlang:binary_to_list(Binary),
	string(String).

string(String) ->
	{_,Tokens,_} = erl_scan:string(String),
	parse_tokens(Tokens,[]).
	
%------------------------------------------------------------
% Helper Functions
%------------------------------------------------------------
parse_tokens([],Acc) -> Acc;
parse_tokens([{'=<',_L}|T],Acc) ->
	parse_tokens(T,Acc++[{'=',_L},{'<',_L}]);
parse_tokens([{Atom,_L}|T],Acc) ->
	parse_tokens(T,Acc++[{Atom,_L}]);
parse_tokens([{string,_L,String}|T],Acc) ->
	parse_tokens(T,Acc++[{string,String,_L}]);
parse_tokens([{atom,_L,Atom}|T],Acc) ->
	Reserved = reserved_word(Atom),
	case Reserved of
		false -> parse_tokens(T,Acc++[{atom,Atom,_L}]);
		true -> parse_tokens(T,Acc++[{Atom}])
	end.
	
reserved_word('copy') -> true;
reserved_word('networkRef') -> true;
reserved_word('move') -> true;
reserved_word('processDict') -> true;
reserved_word('ets') -> true;
reserved_word('processM') -> true;
reserved_word('codeM') -> true;
reserved_word('resource') -> true;
reserved_word('client_code_directories') -> true;
reserved_word('server_evaluation_mode') -> true;
reserved_word('eager') -> true;
reserved_word('lazy') -> true;
reserved_word(_) -> false.
