-module(test).
-export([hello/1]).

hello(Var) ->
    NewVar = 1 + 2,
    Var + NewVar.
