-module(factorial).
-export([fact/1]).

fact(N) ->
    fact(N, 1).

fact(0, Res) ->
    Res;
fact(N, Res) ->
    fact(N - 1, Res * N).
