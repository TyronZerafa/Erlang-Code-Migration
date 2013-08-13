%% ************************************************************
%% Policy Files' grammer definition
%% ************************************************************

Nonterminals rules rule opType resType bindingMech atomLst evalMode.
Terminals '=' ',' '<' '>' ';' '[' ']'
			atom string
			server_evaluation_mode resource codeM processM ets processDict copy networkRef move client_code_directories eager lazy.
Rootsymbol rules.

rules -> rule ';' : '$1'.
rules -> rule ';' rules : ['$1', '$3'].

rule -> 'resource' '=' '<' atom ',' opType ',' resType ',' bindingMech '>' : build_resource({'$1','$4','$6','$8','$10'}).
rule -> 'resource' '=' '<' opType ',' resType ',' bindingMech '>' : build_resource({'$1','$4','$6','$8'}).
			
rule -> 'client_code_directories' '=' '[' atomLst ']' : {directories,'$4'}.

rule -> 'server_evaluation_mode' '=' evalMode : {eval,'$3'}.

atomLst -> string : e(2,'$1').
atomLst -> string ',' atomLst : [e(2,'$1'), '$3']. 

opType -> 'codeM' : '$1'.
opType -> 'processM' : '$1'.

resType -> 'ets' : '$1'.
resType -> 'processDict' : '$1'.

bindingMech -> 'copy' : '$1'.
bindingMech -> 'networkRef' : '$1'.
bindingMech -> 'move' : '$1'.

evalMode -> 'eager' : '$1'.
evalMode -> 'lazy' : '$1'.

Erlang code.

e(2,{_,A,_}) -> A.

build_resource({{resource},{OpType},{ResType},{BindingMech}})->
	{resource,{OpType,ResType,BindingMech}};
build_resource({{resource},{atom,Name,_L},{OpType},{ResType},{BindingMech}})->
	{resource,{Name},{OpType,ResType,BindingMech}}.