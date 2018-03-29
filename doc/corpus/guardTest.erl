-module(guardTest).
-compile(export_all).

seq(First, Last)
    when is_integer(First), is_integer(Last), First-1 =< Last-> 
    seq_loop(Last-First+1, Last, []).

seq_loop(N,M,_)->
    {N,M}.