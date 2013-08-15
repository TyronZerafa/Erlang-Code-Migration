-module(closure_rep).

%% ************************************************************
%% This module constructs the function data value representations.
%% ************************************************************
-import(erlang,[fun_info/2]).
-import(lists,[flatten/1,map/2,nth/2,append/1]).
-import(evaluate,[build_eval_fun/4]).

-export([
			construct_full_fun_rep/2,
			construct_full_fun_rep/5,
			convert_to_lazy/1
		]).

%------------------------------------------------------------
% Interface
%------------------------------------------------------------
%% Constructs the Closure representation of a fun. Any free variables are added as new variables
%% Param = Function, Bindings [{'X',X}] 
construct_full_fun_rep(Fun,Bindings) when erlang:is_function(Fun)->
	%% extract info
	{module, Mod} = fun_info(Fun, module),
	{name, Name} = fun_info(Fun, name),
	{arity, Arity} = fun_info(Fun, arity),
	{type,Type} = fun_info(Fun,type),
	%% get all dependency tree
	Dep = get_dependencies(Mod,Name,Arity,Type),
	%% construct portable function data structure
	get_full_impl(Dep,[{Mod,Name,Arity}],Type,Bindings,[]),
	NewFunName = get_fun_name(Name,Type),
	NewModName = list_to_atom(lists:flatten(io_lib:format("~s-~s@~s",[Mod,NewFunName,node()]))),
	%% pass to future executor
	build_eval_fun(NewModName,NewFunName,convert_vertices_to_list(Dep,digraph:vertices(Dep),[]),Arity).

% Used from compiler for anonymous portable functions
construct_full_fun_rep(Mod,{name,Name},Arity,FunAST,Bindings) ->
	%% get all dependency tree
	Dep = get_dependencies_from_AST(Mod,Name,Arity,FunAST),
	%% construct portable function data structure
	get_full_impl(Dep,[{Mod,Name,Arity}],local,Bindings,[]),
	NewFunName = get_fun_name(Name,local),
	NewModName = list_to_atom(lists:flatten(io_lib:format("~s-~s@~s",[Mod,NewFunName,node()]))),
	%% pass to future executor
	build_eval_fun(NewModName,get_fun_name(Name,local),convert_vertices_to_list(Dep,digraph:vertices(Dep),[]),Arity).

convert_vertices_to_list(_,[],Acc) -> Acc;	
convert_vertices_to_list(Dep,[H|T],Acc) ->
	{H,Label} = digraph:vertex(Dep,H),
	convert_vertices_to_list(Dep,T,Acc ++ [[H,Label,get_new_dependencies(digraph:out_neighbours(Dep,H),[])]]).
%------------------------------------------------------------
% Helper Functions
%------------------------------------------------------------
get_fun_name(Name,external) -> Name;
get_fun_name(Name,local) ->
	list_to_atom(re:replace(atom_to_list(Name), "/", "", [global, {return, list}])).
	
%% Local Fun
construct_fun_closure(Mod,Name,FunArity,Abst,Bindings,local)->
	{FunName,Arity,Rel} = split_name(Name),
	{'fun',_L,{clauses,Clauses}} = 
		extract_fun([construct_closure(Mod,FunName,Arity,Abst)],Rel),
	{function,_L,get_fun_name(Name,local),FunArity,get_clauses(Bindings,Clauses)};

%% External Fun
construct_fun_closure(Mod,Name,Arity,Abst,[],external)->	
	construct_closure(Mod,Name,Arity,Abst).

% Constructs the representation of a function
construct_closure(M,F,A,Abst) ->
	extract_function(parse_transform(Abst,{get_known_funs(M,Abst),node(),M}),F,A).
	
%------------------------------------------------------------
% Abstract Syntax Tree operations
%------------------------------------------------------------

%% Extracts the known functions: i.e. functions belonging that either form part of the AST or imported
get_known_funs(Mod,Abst) ->
	extract_import(Abst)++pick_function_names(Mod,Abst).
	
%% Retrieves the AST of a module
get_mod_abstract_code(Module) ->
   {module,_} = code:ensure_loaded(Module),
   Beam = code:which(Module),
   case beam_lib:chunks(Beam, [abstract_code]) of
	{ok,{_,[{abstract_code,{_,AC}}]}} ->
	    {ok, AC};
	Other ->
	    Other
   end.

%% Retrieves list of exported functions
extract_export(Clauses) ->
	Flatten = fun({attribute,_L,export,_M} = Export) -> Export;
			(_) -> []
			end,
	[X || X <- flatten(map(Flatten, Clauses))].

e({_,[]},Acc) -> Acc;
e({M,[{F,A}|T]},Acc) -> [{M,F,A}] ++ e({M,T},Acc).

%% Retrieves list of imported functions
extract_import(Clauses) ->
	Flatten = fun({attribute,_L,import,M} = _Import) -> e(M,[]);
			(_) -> []
			end,
	[X || X <- flatten(map(Flatten, Clauses))].
		   
%% Retrieves a named function
extract_function(AC,Fun,Arity) ->
	[Clauses] = [Cs || {function,_,F1,Arity1,Cs} <- AC,
		     F1 == Fun, Arity1 == Arity],
	 {function,1,Fun,Arity,Clauses}.	   
	
%% Retrieves an anonymous fun
extract_fun(Clauses,Rel) ->
    Funs = pick_funs(Clauses),
    nth(Rel, Funs).
 
tranverse_fun({'fun',_,{clauses,C}} = Fun) -> tranverse_fun(C) ++ [Fun];
tranverse_fun(T) when erlang:is_tuple(T) -> tranverse_fun(tuple_to_list(T));
tranverse_fun([H|T]) -> tranverse_fun(H) ++ tranverse_fun(T);
tranverse_fun([]) -> [];
tranverse_fun(_) -> [].
	
pick_funs(L) ->
	flatten(map(fun tranverse_fun/1, L)).

pick_function_names(Mod,L) ->
	Flatten = fun({function,_,Name,Arity,_}) -> [{Mod,Name,Arity}];
		(_) -> []
	    end,
	[X || X <- flatten(map(Flatten, L))].

%% Splits the name of an anonymous function
split_name(Name) ->
	[Fs, As, _, Rs] = string:tokens(atom_to_list(Name),"/-"),
	{list_to_atom(Fs), list_to_integer(As), list_to_integer(Rs)+1}.
   
%% Constructs a list of new variables=values from a list of bindings
get_b(Bindings) ->
	get_b(Bindings,[]).
	
get_b([],NewVars) -> NewVars;
get_b([{Var,Value}|T],NewVars)->
	get_b(T,[{match,1,{var,1,Var},{type_of(Value),1,Value}}|NewVars]).

%% Constructs a list of function clauses
get_clauses(Bindings,Clauses)->
	get_clauses(Bindings,Clauses,[]).
	
get_clauses(_Bindings,[],Result) -> Result;
get_clauses(Bindings,[{clause,_L,Param,Guards,Body}|T],Result) ->
	get_clauses(Bindings,T,lists:merge(Result,[{clause,_L,Param,Guards,lists:append(get_b(Bindings),Body)}])).

%% Loops through all Nodes
%% Options = {Imports,Node,_Module,Dep}
parse_transform(AST, Options) ->
    flatten([node_parse(Node, Options) || Node <- AST]).

node_parse(Node, Options) when is_list(Node) ->
    [node_parse(Element, Options) || Element <- Node];

node_parse({call,_,Call,Param},Options) -> 
	node_parse_call(Call,node_parse(Param,Options),Options);
	
node_parse(Node, Options) when is_tuple(Node) ->
    NewNodeTuple = [node_parse(Element, Options) || Element <- tuple_to_list(Node)],
    list_to_tuple(NewNodeTuple);

node_parse(Node, _Options) -> 
	Node.

%% Parse transform node calls
node_parse_call({remote,_,{atom,_,Mod},{atom,_,Fun}},Param,{_Imports,Location,_Module}) ->
	substitute_call(Location,Mod,Fun,Param);
	
node_parse_call({atom,_L2,Fun},Param,{Imports,Location,_Module}) ->
	case get_module(Imports,{Fun,length(Param)}) of 
		[] -> 
			{call,1,{atom,_L2,Fun},Param};
		_Else ->
			substitute_call(Location,_Else,Fun,Param)
		end;
		
node_parse_call({remote,_L2,{var,_L3,VarMod},{var,_L4,VarFun}},Param,{_Imports,Location,_Module}) ->
	substitute_call(Location,{var,_L3,VarMod},{var,_L4,VarFun},Param);

node_parse_call({var,_L,Var},Param,{_Imports,_Location,_Module}) ->
	{call,1,{var,_L,Var},Param};
	
node_parse_call(Else,_Param,{_Imports,_Location,_Module}) ->
	io:format("In node_parse_call encountered ~p ~n",[Else]),
	Else.
	
%% Substitute a call
substitute_call(_ParseTreeNode,Mod,Fun,Param) ->
	Arity = length(Param),
	case mod_belong_to_erlang_lib({Mod, Fun, Arity}) of		
		true -> 
			{call,1,{remote,1,{atom,1,Mod},{atom,1,Fun}},Param};
		_Else ->
			NewModName = list_to_atom(lists:flatten(io_lib:format("~s-~s@~s",[Mod,Fun,node()]))),
						{call,1,{remote,1,{atom,1,NewModName},{atom,1,Fun}},Param}
	end.

%% Retrieves the imported module
get_module([],_FunSpec) -> [];
get_module([{Mod,F,A}|_T], {F,A}) ->
	Mod;
get_module([_Else|T], FunSpec) ->
	get_module(T,FunSpec).

%% Used to determine the type of a function
type_of(X) when is_integer(X)   -> integer;
type_of(X) when is_float(X)     -> float;
type_of(X) when is_list(X)      -> list;
type_of(X) when is_tuple(X)     -> tuple;
type_of(X) when is_bitstring(X) -> bitstring;  % will fail before e12
type_of(X) when is_binary(X)    -> binary;
type_of(X) when is_boolean(X)   -> boolean;
type_of(X) when is_function(X)  -> function;
type_of(X) when is_pid(X)       -> pid;
type_of(X) when is_port(X)      -> port;
type_of(X) when is_reference(X) -> reference;
type_of(X) when is_atom(X)      -> atom;
type_of(_X) 					-> unknown.

%------------------------------------------------------------
% Dependency Management
%------------------------------------------------------------
add_dir(code_analysis_server,[]) -> [];
add_dir(code_analysis_server,[Dir|T]) -> 
	xref:add_directory(code_analysis_server,Dir),
	add_dir(code_analysis_server,T);
add_dir(code_analysis_server,Dir) ->
	xref:add_directory(code_analysis_server,Dir).
	
add_directories(code_analysis_server,[]) -> {ok};
add_directories(code_analysis_server,[{directories,Dir}|T]) ->
	add_dir(code_analysis_server,Dir),
	add_directories(code_analysis_server,T).

get_all_dependencies() ->
	xref:start(code_analysis_server),
	Dir = append(dict:fetch(directories,mcode:get_rules())),
	add_dir(code_analysis_server,Dir),
	%% Retrieve local calls
	{ok,Calls} = xref:q(code_analysis_server, "E"),
	xref:stop(code_analysis_server),
	Calls.	
    
get_dependencies(Mod,Fun,Arity,external) ->
	Calls = get_all_dependencies(),
	DepLst = filter_dependencies([{Mod,Fun,Arity}], Calls),
	convert_to_graph(digraph:new(),DepLst);
	
get_dependencies(Mod,Name,Arity,local) ->
	{FunName,FunArity,Rel} = split_name(Name),
	{ok, Abst} = get_mod_abstract_code(Mod),
	Fun = extract_function(Abst,FunName,FunArity),
	FunAST = [extract_fun([Fun],Rel)],
	get_dependencies_from_AST(Mod,Name,Arity,FunAST).
	
get_dependencies_from_AST(Mod,Name,Arity,FunAST) ->
	{ok, Abst} = get_mod_abstract_code(Mod),
	KnownFuns = get_known_funs(Mod,Abst),
	FirstLevelFunDep = fun_dependency(FunAST,KnownFuns),
	AllFunDep = get_all_dependencies(),
	FilteredFunDep = filter_dependencies(FirstLevelFunDep, AllFunDep),
	Digraph = convert_to_graph( digraph:new(), FilteredFunDep),
	digraph:add_vertex(Digraph,{Mod,Name,Arity}),
	add_edges_to_graph(Digraph,{Mod,Name,Arity},FirstLevelFunDep).

add_edges_to_graph(Digraph,_,[]) -> Digraph;
add_edges_to_graph(Digraph,F,[H|T]) ->
	digraph:add_vertex(Digraph,H),
	digraph:add_edge(Digraph,{F,H},F,H,lists:flatten(io_lib:format("~p to ~p",[F,H]))),
	add_edges_to_graph(Digraph,F,T).

%% Converts a list of {{Callee},{Call}} into a directed graph
convert_to_graph(Digraph, []) -> Digraph;
convert_to_graph(Digraph, [{A,B}|T]) ->
	digraph:add_vertex(Digraph,B),
	digraph:add_vertex(Digraph,A),
	digraph:add_edge(Digraph,{A,B},A,B,lists:flatten(io_lib:format("~p to ~p",[A,B]))),
	convert_to_graph(Digraph, T).

%% By AST
unresolved_dependency({call,_,{remote,_,{atom,_,'$M_EXPR'},{atom,_,_}},_}) -> true;
unresolved_dependency({call,_,{remote,_,{atom,_,_},{atom,_,'$F_EXPR'}},_}) -> true;
unresolved_dependency({call,_,{remote,_,{atom,_,_},{atom,_,_}},-1}) -> true;
unresolved_dependency({call,_,{atom,_,'$F_EXPR'},_}) -> true;
unresolved_dependency({call,_,_,-1}) -> true;
%% By M:F/A
unresolved_dependency({'$M_EXPR',_,_}) -> true;
unresolved_dependency({_,'$F_EXPR',_}) -> true;
unresolved_dependency({_,_,-1}) -> true;
unresolved_dependency({'$F_EXPR',_}) -> true;
unresolved_dependency({_,-1}) -> true;
unresolved_dependency(_) -> false.

fun_dependency(AST,KnownFuns)-> fun_dependency(AST,KnownFuns,[]).

%% Skip unresolved dependencies
fun_dependency([],_,Acc) -> Acc;
fun_dependency({call,_L,_MF,_P}=Call,_KnownFun,Acc) -> 
	case unresolved_dependency(Call) of
		true -> io:format("Unresolved dependency"), Acc;
		_Else -> call_dependency(Call,_KnownFun,Acc)
	end;

fun_dependency(A,_KnownFun,Acc) when is_tuple(A) -> fun_dependency(tuple_to_list(A),_KnownFun,Acc);
fun_dependency([H|T],_KnownFun,Acc) -> fun_dependency(T,_KnownFun,fun_dependency(H,_KnownFun,Acc));
fun_dependency(_,_,Acc)-> Acc.

call_dependency({call,_,{remote,_,{atom,_,Mod},{atom,_,Fun}},Param},_,Acc) -> Acc++[{Mod,Fun,length(Param)}];
call_dependency({call,_,{atom,_L2,Fun},Param},KnownFun,Acc) -> Acc++[{get_module(KnownFun,{Fun,length(Param)}),Fun,length(Param)}];
call_dependency({call,_,A,_Param},_KnownFun,Acc) -> io:format("Encountered Call ~p ~n",[A]), Acc.

filter_dependencies(Calls,Dep) -> 
	{FilteredDep,_} = filter_dependencies(Calls,Dep,{[],[]}),
	FilteredDep.

filter_dependencies([],_,Acc) -> Acc;
filter_dependencies([H|AccCalls],AllDependencies,{Acc,ProcessedCallers}) ->
	SecondLevelAcc =
		case lists:member(H,ProcessedCallers) of
			false ->
				HDepAcc = extract_dependencies_of(H,AllDependencies,Acc),
				filter_dependencies(get_callees(HDepAcc,ProcessedCallers++[H]),AllDependencies,{HDepAcc,ProcessedCallers++[H]});
			_Else ->
				{Acc,ProcessedCallers}
		end,		
	filter_dependencies(AccCalls,AllDependencies,SecondLevelAcc).
	
extract_dependencies_of(_,[],Processed) -> Processed;
extract_dependencies_of(Caller,[{Caller,Callee}|RemainingDep],Processed) ->
	AlreadyAdded = general_utils:list_contain_element(Processed,Callee),
	ErlangLib = mod_belong_to_erlang_lib(Callee),
	Unresolved = unresolved_dependency(Callee),	
	Acc = case AlreadyAdded or ErlangLib or Unresolved of
				false -> Processed++[{Caller,Callee}];
				_Else -> Processed
    end,
	extract_dependencies_of(Caller,RemainingDep,Acc);
extract_dependencies_of(Caller,[_|RemainingDep],Processed) ->	
	extract_dependencies_of(Caller,RemainingDep,Processed).

get_callees([],Acc) -> Acc;
get_callees([{_,Callee}|T],Acc) -> 
	get_callees(T,Acc++[Callee]).

% Checks whether a module belong to the Erlang Library
mod_belong_to_erlang_lib({Mod,_Fun,_Arity}) ->
	
	code:add_path("/usr/local/lib/erlang/lib"),
	Path = code:where_is_file(lists:flatten(io_lib:format("~p.beam",[Mod]))),
	RegExp = "[a-zA-Z\:\/]*(\/lib\/)+[a-zA-Z\:\/]*",
	Options = [],
	
	case Path of
		non_existing -> 
			false;
		_Else ->	
			case re:run(Path, RegExp, Options) of
				{match,_} -> true ;
				nomatch -> false
			end
	end.

%------------------------------------------------------------
% Portable Function Implementation Combination
%------------------------------------------------------------
list_contain_element([],_Fun) -> false;
list_contain_element([Fun|_T],Fun) -> true;
list_contain_element([_A|T],Fun) -> list_contain_element(T,Fun).

get_full_impl(_DepTree,[],_Type,_Bindings,_Processed) -> ok;
get_full_impl(DepTree,[Root={M,F,A}|T],Type,Bindings,Processed) ->
	ProcessedFlag = list_contain_element(Processed,Root),
	case ProcessedFlag of
		false ->
			Dep = digraph:out_neighbours(DepTree,Root),
			FunCode = [{attribute,1,module,Mod},{attribute,1,export,[{FunName,Arity}]},_FunCode,_] = get_fun_impl(Root,Type,Bindings),
			substitute_label(DepTree,{M,F,A},{Mod,FunName,Arity},FunCode),
			get_full_impl(DepTree,Dep,external,[],Processed ++ [Root] ++ [{Mod,FunName,Arity}]),
			get_full_impl(DepTree,T,Type,Bindings,Processed ++ [Root] ++ [{Mod,FunName,Arity}] ++ Dep);
		true ->
			get_full_impl(DepTree,T,Type,Bindings,Processed)
	end.

substitute_label(D,OldVertex,NewVertex,NewLabel) ->
	digraph:add_vertex(D,NewVertex,NewLabel),
	OutEdges = digraph:out_neighbours(D,OldVertex),
	InEdges = digraph:in_neighbours(D,OldVertex),
	RecursiveFun = list_contain_element(InEdges,OldVertex),
	if RecursiveFun -> 
			add_in_edges(D,[NewVertex],NewVertex);
		true -> []
	end,
	add_edges(D,NewVertex,OutEdges),
	add_in_edges(D,InEdges,NewVertex),
	digraph:del_vertex(D,OldVertex).
	
add_edges(Digraph,_Root,[]) -> Digraph;
add_edges(Digraph,Root,[H|T]) -> 
	digraph:add_edge(Digraph,Root,H),
	add_edges(Digraph,Root,T).
	
add_in_edges(Digraph,[],_Root) -> Digraph;
add_in_edges(Digraph,[H|T],Root) -> 
	digraph:add_edge(Digraph,H,Root),
	add_in_edges(Digraph,T,Root).

get_fun_impl({Mod,Name,Arity},Type,Bindings) ->
	{ok, Abst} = get_mod_abstract_code(Mod),
	{function,_,NewFunName,_,_} = NewFunCode = construct_fun_closure(Mod,Name,Arity,Abst,Bindings,Type),
	NewMod = list_to_atom(lists:flatten(io_lib:format("~s-~s@~s",[Mod,NewFunName,node()]))),
	[{attribute,1,module,NewMod},{attribute,1,export,[{NewFunName,Arity}]},NewFunCode,{eof,100}].

get_new_dependencies([],Acc) -> Acc;
get_new_dependencies([{M,F,A}|T],Acc) ->
	get_new_dependencies(T,Acc ++ [{M,F,A}]).

%------------------------------------------------------------
% Lazy Conversion
%------------------------------------------------------------
convert_to_lazy(AST) ->
	flatten([node_parse_lazy(Node) || Node <- AST]).

node_parse_lazy(Node) when is_list(Node) ->
    [node_parse_lazy(Element) || Element <- Node];

node_parse_lazy({call,_L,Call,Param}) -> 
	node_parse_lazy_call({call,_L,Call,node_parse_lazy(Param)});
	
node_parse_lazy(Node) when is_tuple(Node) ->
    NewNodeTuple = [node_parse_lazy(Element) || Element <- tuple_to_list(Node)],
    list_to_tuple(NewNodeTuple);

node_parse_lazy(Node) -> 
	Node.

node_parse_lazy_call({call,_,{remote,_,{atom,_,Mod},{atom,_,Fun}},Param}) ->
	case mod_belong_to_erlang_lib({Mod, Fun, length(Param)}) of		
		false -> {call,1,{remote,1,{atom,1,mcode_cb},{atom,1,load_execute_fun}},[{atom,1,Mod},{atom,1,Fun},{atom,1,Param}]};
		true -> {call,1,{remote,1,{atom,1,Mod},{atom,1,Fun}},Param}
	end;
	
node_parse_lazy_call({call,_,{remote,_L2,{var,_L3,VarMod},{var,_L4,VarFun}},[ParamArrayAttr]=Param}) ->
	case mod_belong_to_erlang_lib({VarMod, VarFun, length(Param)}) of	
		false -> {call,1,{remote,1,{atom,1,mcode_cb},{atom,1,load_execute_fun}},[{var,_L3,VarMod},{var,_L4,VarFun},ParamArrayAttr]};
		true -> {call,1,{remote,1,{var,1,VarMod},{var,1,VarFun}},[ParamArrayAttr]}
	end;

%% Anonymous Fun	
node_parse_lazy_call({call,_,{var,_,Var},Param}) ->
	{call,1,{var,1,Var},Param};

%% Function belonging to this mod
node_parse_lazy_call({call,_,{atom,_,Fun},Param}) ->
	{call,1,{atom,1,Fun},Param};
	
node_parse_lazy_call({call,_,Else,Param}) ->
	io:format("In node_parse_call encountered ~p ~n",[Else]),
	{call,1,Else,Param}.
