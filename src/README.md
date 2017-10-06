# Jrlscript Compiler Source Code

## Coregen:
Coregen is an Erlang module which can compile normal Erlang source code files into both CoreErlang and a CoreErlang AST. This can be simply done via:

``` 
1) Compiling the coregen.erl file in Erlang (i.e via c("coregen"). in the Erlang Shell)
2) Running either the coregen:er2ce(...) function or the coregen:er2ast(...) function, both of
   which take a module name as per the BIF c(...), i.e

       coregen:er2ce("somemodule"). or coregen:er2ast("somemodule").

   Which will generate the files "somemodule.core" and "somemodule.ast" respectively
3) Feeding the atom 'true' to the er2ast function will delete any resultant core files.
```
