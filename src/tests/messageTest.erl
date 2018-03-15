-module(messageTest).
-compile(export_all).

send_request(Pid,Req)->
    Pid ! {io_request,self(),Req}.

receive_request()->
    % io:format("A",[]),
    receive
    {io_request,Pid,Data}->
        io:format("Data: ~p~n",[Data]);
    {_,Pid,Data}->
        io:format("Error",[])
    end.

receive_request2()->
    % io:format("A",[]),
    receive
    {io_request,Pid,Data}->
        io:format("Data: ~p~n",[Data]);
    {_,Pid,Data}->
        io:format("Error",[])
    end,
    io:format("End").

receive_request3()->
    % io:format("A",[]),
    receive
        _->
            io:format("Something",[])
    after
        1100 ->
            io:format("Timeout",[])
    end.

receive_request4()->
    % io:format("A",[]),
    receive
        _->
            io:format("Something",[])
    after
        0 ->
            io:format("Message queue was empty",[])
    end.