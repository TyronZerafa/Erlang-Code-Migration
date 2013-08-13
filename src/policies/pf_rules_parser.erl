%% ************************************************************
%% Policy Files' rules parser
%% ************************************************************
-module(pf_rules_parser).

-export([compile_rules/1,compile_rules_from_file/1]).

%------------------------------------------------------------
% Interface
%------------------------------------------------------------
compile_rules(Rules) when is_list(Rules)->
	compile_rules(Rules,dict:new());
compile_rules(Rules) ->
	compile_rules([Rules],dict:new()).

compile_rules_from_file(PolicyFile) ->
	Tokens = pf_lexer:file(PolicyFile),
	{ok,Rules} = pf_gf:parse(Tokens),
	compile_rules(Rules).
	
%------------------------------------------------------------
% Helper Functions
%------------------------------------------------------------
compile_rules([],Dict) -> Dict;
compile_rules([{directories,Directories}|T],Dict) -> compile_rules(T,dict:append(directories,Directories,Dict));
compile_rules([{eval,{EvalMode}}|T],Dict) -> compile_rules(T,dict:append(eval,EvalMode,Dict)).	
