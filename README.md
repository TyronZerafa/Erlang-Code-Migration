Erlang - Code Migration
=====================

****
Authors: Tyron Zerafa, Adrian Francalanza
****

Erlang's distribution mechanism offers functionality that facilitates the creation of processes over remote nodes 
via the ```spawn``` BIF. The semantics of this remote process creation require the service node (upon which the new 
process will be created) to have the necessary codebase, i.e., the set of modules and function deﬁnitions; 
otherwise evaluation would terminate with an undefined function exception. Erlang provides functionality that loads 
code onto remote nodes; however, as discussed in [Towards an Abstraction for Remote Evaluation 
in Erlang] (http://staff.um.edu.mt/afra1/papers/ew2013.pdf) presented in Erlang Workshop 2013, this may be 
infeasible or even undesirable giving rise to a number of problems ranging from codebase name clashes to unknown 
dependencies at compile-time. This Erlang extension, discussed in [Code Management Automation for Erlang Remote Actors]
(http://staff.um.edu.mt/afra1/papers/agere2013.pdf), takes care to load the least required code during a remote process
creation in either an eager or lazy mode.

## Remote-Evaluation: Use Case ##

Consider the following simple Erlang modules located on node 'NodeL'.

```
%% Module performing basic math operations 
-module(bsc_math).
-export([mult/2]).

mult(X,Y) -> X * Y.

%% Module performing complex math operations 
-module(cpx_math).
-export([fac/1, fib/1]).

fac(0) -> 1;
fac(X) -> bsc_math:mult(X,fac(X-1)).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).
```

Using this extension, 'NodeL' can create a remote process that computes the ```cpx_math:fac/1``` on 'NodeM' using 
the following call: ```erlang:spawn(NodeM,cpx_math,fac,[6])```. This would first check whether the required code resides on 
the remote node, remotely load any missing functions and initiates execution. The result of such operation may be 
requested from 'NodeM' via a new call; ```erlang:get_result(Pid)``` where 'Pid' is the process identifier of the remotely 
created process as returned by ```erlang:spawn/4```. 

The same load/ remotely execute semantics may also be achieved for anonymous functions through the use of portable 
functions (discussed later). Basically, a portable function is a funtion that holds its own implementation rather than a mere 
symbolic reference to facilitate distribution of code between different Erlang nodes. For instance 
```erlang:spawn(NodeM, pfun() -> fac(6) + fib(2) end)``` would load and execute the anonymous portable function 
on ERTS called 'NodeM'.  

Application developers may also decide whether to load portable functions eagerly or lazily. Under an eager loading 
scheme, all required code gets transferred and loaded inside a remote node prior to evaluation initialization. 
Under a lazy loading scheme, only the root function gets loaded initially and other functions gets transferred 
when they are needed. This scheme ensures that only the least required code gets transferred between different nodes,
saving bandwidth and storage overheads in the presence of branches. The loading scheme may be set inside a file 
'policy_file.conf' located under the 'ebin' folder.

## Portable Functions ##

The introduced code migration mechanism used in remote process creation is based on portable functions. These 
were first proposed in [EEP (Erlang Enhanced Proposal)](http://www.erlang.org/eeps/eep-0015.html) 
as a restricted type of functions that include their own source code rather than the mere (symbolic) code references 
attributed to local and external functions. 

### Creation ###
A portable function can be easily created using a new ```pfun``` keyword, similar to the Erlang's native 'local' 
and 'external' functions. For instance ``` F = pfun cpx_math:fac/1``` creates a new named portable function, 'F', 
that contains the semantically equivalent code of 'cpx_math:fac/1'. Similarly, ```F = pfun(X) -> fac(X) + fib(X) end``` 
creates a new named portable function that contains the semantically equivalent code.

### Inspection ###
The contents of a portable function can be inspected using ```erlang:fun_info/1``` and ```erlang:portable_fun_info/2``` 
as follows.

```
> erlang:fun_info(pfun(X) -> fac(X) + fib(X) end).

[{module,'cpx_math--test0-fun-0-@A@tyron-VirtualBox'},
 {name,'-test0-fun-0-'},
 {arity,1},
 {code,
     [{attribute,1,module,'cpx_math--test0-fun-0-@A@tyron-VirtualBox'},
      {attribute,1,export,[{'-test0-fun-0-',1}]},
      {function,12,'-test0-fun-0-',1,
          [{clause,12,
               [{var,12,'X'}],
               [],
               [{op,12,'+',
                    {call,1,
                        {remote,1,
                            {atom,1,'cpx_math-fac@A@tyron-VirtualBox'},
                            {atom,1,fac}},
                        [{var,12,'X'}]},
                    {call,1,
                        {remote,1,
                            {atom,1,'cpx_math-fib@A@tyron-VirtualBox'},
                            {atom,1,fib}},
                        [{var,12,'X'}]}}]}]},
      {eof,100}]},
 {calls,
     [{'cpx_math-fac@A@tyron-VirtualBox',fac,1},
      {'cpx_math-fib@A@tyron-VirtualBox',fib,1}]},
 {type,portable}]

```
This function data value contains the portable function's code abstract syntax tree (AST) in the 'code' tag. 
The 'calls' tag lists the AST's dependencies - the dependencies code is also stored inside the portable function definition 
and can be accessed via a call to ```erlang:portable_fun_info/2```. For instance, 
the code of dependency ```'cpx_math-fac@A@tyron-VirtualBox':fac/1``` can be retrieved by
```erlang:portable_fun_info(pfun(X) -> fac(X) + fib(X) end,{dependency_code,{'cpx_math-fac@A@tyron-VirtualBox',fac,1}})```. 
Similarly, the dependendencies of this call may also be retrieved by using item 'dependency_dep' instead of 'depenency_code'.
 
The 'type' tag indicates that the inspected function is portable - this can also be determined via a call to 
```erlang:is_function_portable/1``` which returns a boolean value.
 
### Execution ###
Portable functions can be evaluated the same as 'local'/ 'external' functions. Upon execution, a portable function 
compiles and loads all its ASTs inside the ERTS and starts executing from the root function identified by the 
'module', 'name' and 'arity' tags. The 'module' name changes that can be seen in the portable functions' AST ensure
that the original modules in which portable functions are constructed do not get replaced inside the Erlang code server.

## Installation ##
The main objective of this work is to extend the Erlang langauge with code migration capabailities which are used in 
the development of remote evaluation. We have decided to introduce new keywords and change the ```spawn``` semantics 
rather than introducing a new library to ease adoption and use. This requires users to compile and replace Erlang native 
modules located inside the 'otp-changes' folder and calling 'sudo make install' which goes through Erlang's makefiles 
and reinstalls the it with the new BEAM files. Furthermore, one need to start the code migration manager by 
executing ```application:start(remote_evaluation)```.

Note that by replacing the indicated BEAM files you will be changing the native ```spawn``` semanics. Furthermore, 
this work does not support native compiled code (using HiPE) due to (code) portability reasons. Code is extracted 
from BEAM files that have been compiled with the 'debug_info' flag; code of modules whose BEAM files were compiled 
differently will not be included in the portable function implementation, assuming that it will be present (and 
dynamically linked) on the remote location.
