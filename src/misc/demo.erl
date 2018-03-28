-module(demo).
-compile(export_all).

stdout_example(String) ->
    io:format("~s", String).

arith_example() ->
    A = 13 * 5,
    B = 99.5 / 12,
    C = 3 div 2,
    D = 12 rem 4,
    E = 15 rem 4,
    io:format("~p, ~p, ~p, ~n ~p, ~p", [A, B, C, D, E]).

list_example(A,B,C)->
    io:format("~p",[[[A,[B]],C]]).

tuple_example(A,B,C)->
    io:format("~p",[{{A,{B}},C}]).

pattern_matching([_|Tail],{apple,_,_})->
    io:format("~p~n~p",[Tail,apple]).

%####################################################
fact(N) ->
    io:format("~p",[fact(N, 1)]).

fact(0, Res) ->
    Res;
fact(N, Res) ->
    fact(N - 1, Res * N).

%####################################################
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

fibbonaci(N)->
    io:format("~p",[fib(N)]).

%####################################################
ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_PID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    Pong_PID = spawn(?MODULE, pong, []),
    spawn(?MODULE, ping, [3, Pong_PID]).