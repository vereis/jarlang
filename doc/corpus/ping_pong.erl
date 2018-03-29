-module(ping_pong).
-export([start/0,  ping/2, pong/0]).
%%% ---------------------------------------------------------------------------------------------%%%
%%% - The ping pong example from http://erlang.org/doc/getting_started/conc_prog.html -----------%%%
%%% ---------------------------------------------------------------------------------------------%%%
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
    Pong_PID = spawn(ping_pong, pong, []),
    spawn(ping_pong, ping, [3, Pong_PID]).