Erlang - Code Migration
=====================
An Erlang extension introducing portable functions which can be transferred between different nodes.

****
Authors: Tyron Zerafa, Adrian Francalanza
****

## Portable Functions ##

Portable functions were first proposed in [EEP (Erlang Enhanced Proposal)](http://www.erlang.org/eeps/eep-0015.html) 
as a restricted type of functions that include their own source code rather than the mere (symbolic) code references 
attributed to local and external functions. 

Consider the following simple Erlang modules.

### Use Case ###

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

### Remote Evaluation ###
Portable functions facilitate the distribution of code between different Erlang nodes. A portable function may be passed 
as an argument to the Erlang spawn mechanism to initiate the execution of functions whose code may be missing on the 
remote location. For instance ```erlang:spawn(NodeK,pfun(X) -> fac(X) + fib(X) end,[6])``` would execute the portable 
function on ERTS called 'NodeK', even if this node misses the required modules. The result of such operation may be 
requested from 'NodeK' via a new call ```erlang:get_result(Pid)``` where 'Pid' is the process identifier of the remotely 
spaywned process as returned by ```erlang:spawn/3```.

Application developers may also decide whether to load portable functions eagerly or lazily. Under an eager loading scheme,
all ASTs gets transferred and loaded inside a remote node prior to evaluation initialization. Under a lazy loading scheme,
only the root function gets loaded initially and other ASTs gets transferred when theneeded. This scheme ensures that only 
the least required code gets transferred between different nodes saving bandwidth and storage overheads in the presence of
branches. The loading scheme may be set inside a file 'policy_file.conf' located under the '/include' dir. 

## Installation ##



