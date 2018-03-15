-module(seqTest).
-compile(export_all).


seq(First, Last, Inc) 
    when is_integer(First), is_integer(Last), is_integer(Inc) -> 
    if
        Inc > 0, First - Inc =< Last,%;
        Inc < 0, First - Inc >= Last ->
            N = (Last - First + Inc) div Inc,
            seq_loop(N, Inc*(N-1)+First, Inc, []);
        Inc =:= 0, First =:= Last ->
            seq_loop(1, First, Inc, [])
    end.

seq_loop(N, X, D, L) when N >= 4 ->
     Y = X-D, Z = Y-D, W = Z-D,
     seq_loop(N-4, W-D, D, [W,Z,Y,X|L]);
seq_loop(N, X, D, L) when N >= 2 ->
     Y = X-D,
     seq_loop(N-2, Y-D, D, [Y,X|L]);
seq_loop(1, X, _, L) ->
     [X|L];
seq_loop(0, _, _, L) ->
     L.
