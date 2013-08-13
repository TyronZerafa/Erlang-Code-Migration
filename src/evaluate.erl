-module(evaluate).

-compile(export_all).

%------------------------------------------------------------
% Portable Function Executors
%------------------------------------------------------------
%% A necessary hack
build_eval_fun(ModName,FunName,CodeLst,0) -> fun() -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[]) end;
build_eval_fun(ModName,FunName,CodeLst,1) -> fun(A) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A]) end;
build_eval_fun(ModName,FunName,CodeLst,2) -> fun(A,B) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B]) end;
build_eval_fun(ModName,FunName,CodeLst,3) -> fun(A,B,C) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C]) end;
build_eval_fun(ModName,FunName,CodeLst,4) -> fun(A,B,C,D) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D]) end;
build_eval_fun(ModName,FunName,CodeLst,5) -> fun(A,B,C,D,E) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E]) end;
build_eval_fun(ModName,FunName,CodeLst,6) -> fun(A,B,C,D,E,F) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F]) end;
build_eval_fun(ModName,FunName,CodeLst,7) -> fun(A,B,C,D,E,F,G) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G]) end;
build_eval_fun(ModName,FunName,CodeLst,8) -> fun(A,B,C,D,E,F,G,H) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H]) end;
build_eval_fun(ModName,FunName,CodeLst,9) -> fun(A,B,C,D,E,F,G,H,I) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I]) end;
build_eval_fun(ModName,FunName,CodeLst,10) -> fun(A,B,C,D,E,F,G,H,I,J) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J]) end;
build_eval_fun(ModName,FunName,CodeLst,11) -> fun(A,B,C,D,E,F,G,H,I,J,K) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K]) end;
build_eval_fun(ModName,FunName,CodeLst,12) -> fun(A,B,C,D,E,F,G,H,I,J,K,L) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K,L]) end;
build_eval_fun(ModName,FunName,CodeLst,13) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K,L,M]) end;
build_eval_fun(ModName,FunName,CodeLst,14) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) end;
build_eval_fun(ModName,FunName,CodeLst,15) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) end;
build_eval_fun(ModName,FunName,CodeLst,16) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) end;
build_eval_fun(ModName,FunName,CodeLst,17) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) end;
build_eval_fun(ModName,FunName,CodeLst,18) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) end;
build_eval_fun(ModName,FunName,CodeLst,19) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) end;
build_eval_fun(ModName,FunName,CodeLst,20) -> fun(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) -> compile_load_all(CodeLst), erlang:apply(ModName,FunName,[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) end.

compile_load_all([]) ->	[];
compile_load_all([[{_M,_F,_A},C,_D]|OtherPFuns]) ->
	compile_load_all(OtherPFuns),
	load_function(C).
	
load_function([]) -> [];	
load_function(Code) ->
	{ok,_Mod,Bin} = compile:forms(Code,[binary]),
	code:load_binary(_Mod,'', Bin).
