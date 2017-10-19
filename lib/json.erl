-module(json).
-export([
            toJson/0,
            toJson/1
        ]).

% -- Macros   --
% is_float(X) ; is_integer(X) is ugly
-define(is_number(Value), is_float(Value) ; is_integer(Value)).

% Nulls/Undefined are the only atom besides true and false which can be stored in json
-define(is_allowed_atom(Value), Value =:= null ; Value =:= undefined ; is_boolean(Value)).



% -- Functions --
% Takes a Erlang Map and outputs a JSON String representation of it
toJson() ->
    {err, no_object}.

toJson(Object) when is_map(Object) ->
    case maps:size(Object) > 0 of
        true ->
            Buffer = print("{~n", [], 0, []),
            Json = lists:flatten(print("}~n", [], 0, consumeObj(Object, 1, Buffer))),
            Json;
        false ->
            "\{\}~n"
    end;

toJson(Array) when is_list(Array) ->
    case length(Array) > 0 of
        true ->
            Buffer = print("[~n", [], 0, []),
            Json = lists:flatten(print("]~n", [], 0, consumeArr(Array, 1, Buffer))),
            Json;
        false ->
            "\[\]~n"
    end.

% Reads each node of a list and builds JSON String
consumeArr([], _Depth, Buffer) ->
    Buffer;

consumeArr([Value], Depth, Buffer) ->
   consumeArrIndex(Value, Depth, Buffer);

consumeArr([Value | Tail], Depth, Buffer) ->
    consumeArr(Tail, Depth, consumeArrIndex(Value, Depth, Buffer)).


% Consumes a given array index and performs different string manipulations based on type
consumeArrIndex(Value, Depth, Buffer) when is_map(Value) ->
    case maps:size(Value) > 0 of
        true ->
            AfterOpenBrace = print("{~n", [], Depth, Buffer),
            AfterNodeContents = consumeObj(Value, Depth + 1, AfterOpenBrace),
            AfterCloseBrace = print("},~n", [], Depth, AfterNodeContents),
            AfterCloseBrace; 
        false ->
            print("{},~n", [], Depth, Buffer)
    end;

consumeArrIndex(Value, Depth, Buffer) when is_list(Value) ->
    case length(Value) > 0 of
        true ->
            AfterOpenBrace = print("[~n", [], Depth, Buffer),
            AfterNodeContents = consumeArr(Value, Depth + 1, AfterOpenBrace),
            AfterCloseBrace = print("],~n", [], Depth, AfterNodeContents),
            AfterCloseBrace; 
        false ->
            print("[],~n", [], Depth, Buffer)
    end;

consumeArrIndex(Value, Depth, Buffer) when ?is_number(Value) ; ?is_allowed_atom(Value) ->
    print("~w,~n", [Value], Depth, Buffer);

consumeArrIndex(Value, Depth, Buffer) when is_bitstring(Value) ->
    print("\"~s\",~n", [bitstring_to_list(Value)], Depth, Buffer);

consumeArrIndex(Value, Depth, Buffer) ->
    print("\"~w\", // Unsupported datatype converted to string to try and not break JSON~n", [Value], Depth, Buffer).


% Reads each node of a map and builds JSON String
consumeObj(Object, Depth, Buffer) when map_size(Object) > 1 ->
    [{Key, Value} | Tail] = maps:to_list(Object),
    consumeObj(maps:from_list(Tail), Depth, consumeNode(Key, Value, Depth, Buffer));

consumeObj(Object, Depth, Buffer) ->
    [{Key, Value}] = maps:to_list(Object),
    consumeNode(Key, Value, Depth, Buffer).


% Consumes a given node and performs different string manipulations based on type
consumeNode(Key, Value, Depth, Buffer) when is_map(Value) ->
    case maps:size(Value) > 0 of
        true ->
            AfterOpenBrace = print("\"~s\": {~n", [Key], Depth, Buffer),
            AfterNodeContents = consumeObj(Value, Depth + 1, AfterOpenBrace),
            AfterCloseBrace = print("},~n", [], Depth, AfterNodeContents),
            AfterCloseBrace;
        false ->
            print("\"~s\": {}~n", [Key], Depth, Buffer)
    end;

consumeNode(Key, Value, Depth, Buffer) when is_list(Value) ->
    case length(Value) > 0 of
        true ->
            AfterOpenBrace = print("\"~s\": [~n", [Key], Depth, Buffer),
            AfterNodeContents = consumeArr(Value, Depth + 1, AfterOpenBrace),
            AfterCloseBrace = print("],~n", [], Depth, AfterNodeContents),
            AfterCloseBrace;
        false ->
            print("\"~s\": []~n", [Key], Depth, Buffer)
    end;

consumeNode(Key, Value, Depth, Buffer) when ?is_number(Value) ; ?is_allowed_atom(Value) ->
    print("\"~s\": ~w,~n", [Key, Value], Depth, Buffer);

consumeNode(Key, Value, Depth, Buffer) when is_bitstring(Value) ->
    print("\"~s\": \"~s\",~n", [Key, bitstring_to_list(Value)], Depth, Buffer);

consumeNode(Key, Value, Depth, Buffer) ->
    print("\"~s\": \"~w\", // Unsupported datatype converted to string to try and not break JSON~n", [Key, Value], Depth, Buffer).


% Functions which indents and prints
print(String, Vars, 0, Buffer) ->
    % io:format(String, Vars),
    NewBuffer = io_lib:format("~s" ++ String, [Buffer] ++ Vars),
    NewBuffer;
print(String, Vars, Depth, Buffer) ->
    % io:format((lists:flatten(lists:map(fun(_) -> "\t" end, lists:seq(1, Depth)))) ++ String, Vars),
    NewBuffer = io_lib:format("~s" ++ (lists:flatten(lists:map(fun(_) -> "\t" end, lists:seq(1, Depth)))) ++ String, [Buffer] ++ Vars),
    NewBuffer.
