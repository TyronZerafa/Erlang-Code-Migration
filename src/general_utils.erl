-module(general_utils).

-export([
			convert_to_erl_source/2, 
			convert_to_abstract_form/1, 
			list_contain_element/2,
			convert_to_erl_source_exprs/1
		]).

%% List contain call
list_contain_element( [], _Anything) -> false; 
list_contain_element( [Call|_T], Call) -> true;
list_contain_element( [{}|Call], Call) -> true;
list_contain_element( [_Else|T], Call) -> list_contain_element(T,Call).

%% Converts erlang source attributes to erlang source
convert_to_erl_source( [], Acc) -> lists:reverse(Acc);
convert_to_erl_source( [H|T], Acc)->
	Strings = lists:flatten(erl_pp:form(H)),
	convert_to_erl_source(T,[Strings|Acc]).

convert_to_erl_source_exprs(H)->
	lists:flatten(erl_pp:exprs(H)).

%% Converts a piece of erlang code into its excerpt representation
convert_to_abstract_form(CodeExcerpt) -> 
	{ok, Tokens,_} = erl_scan:string(CodeExcerpt),
	erl_parse:parse_form(Tokens).
