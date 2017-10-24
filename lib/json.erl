-module(json).
-export([
            serialize/0,
            serialize/1
        ]).

% -- Macros   --
% is_float(X) ; is_integer(X) is ugly
-define(IS_NUMBER(Value), is_float(Value) ; is_integer(Value)).

% Nulls/Undefined are the only atom besides true and false which can be stored in json
-define(IS_ALLOWED_ATOM(Value), Value =:= null ; Value =:= undefined ; is_boolean(Value)).



% -- Functions --
% Takes a Erlang Map and outputs a JSON String representation of it
serialize() ->
    {err, no_object}.

serialize(Object) when is_map(Object) ->
    case maps:size(Object) > 0 of
        true ->
            Buffer = format("{~n", [], 0, []),
            Json = lists:flatten(format("}~n", [], 0, consume_obj(Object, 1, Buffer))),
            Json;
        false ->
            "\{\}~n"
    end;

serialize(Array) when is_list(Array) ->
    case length(Array) > 0 of
        true ->
            Buffer = format("[~n", [], 0, []),
            Json = lists:flatten(format("]~n", [], 0, consume_arr(Array, 1, Buffer))),
            Json;
        false ->
            "\[\]~n"
    end.

% Reads each node of a list and builds JSON String
consume_arr([], _Depth, Buffer) ->
    Buffer;

consume_arr([Value], Depth, Buffer) ->
   consume_arr_index(Value, Depth, Buffer);

consume_arr([Value | Tail], Depth, Buffer) ->
    consume_arr(Tail, Depth, consume_arr_index(Value, Depth, Buffer)).


% Consumes a given array index and performs different string manipulations based on type
consume_arr_index(Value, Depth, Buffer) when is_map(Value) ->
    case maps:size(Value) > 0 of
        true ->
            AfterOpenBrace = format("{~n", [], Depth, Buffer),
            AfterNodeContents = consume_obj(Value, Depth + 1, AfterOpenBrace),
            AfterCloseBrace = format("},~n", [], Depth, AfterNodeContents),
            AfterCloseBrace; 
        false ->
            format("{},~n", [], Depth, Buffer)
    end;

consume_arr_index(Value, Depth, Buffer) when is_list(Value) ->
    case length(Value) > 0 of
        true ->
            AfterOpenBrace = format("[~n", [], Depth, Buffer),
            AfterNodeContents = consume_arr(Value, Depth + 1, AfterOpenBrace),
            AfterCloseBrace = format("],~n", [], Depth, AfterNodeContents),
            AfterCloseBrace; 
        false ->
            format("[],~n", [], Depth, Buffer)
    end;

consume_arr_index(Value, Depth, Buffer) when ?IS_NUMBER(Value) ; ?IS_ALLOWED_ATOM(Value) ->
    format("~w,~n", [Value], Depth, Buffer);

consume_arr_index(Value, Depth, Buffer) when is_bitstring(Value) ->
    format("\"~s\",~n", [bitstring_to_list(Value)], Depth, Buffer);

consume_arr_index(Value, Depth, Buffer) ->
    format("\"~w\", // Unsupported datatype converted to string to try and not break JSON~n", [Value], Depth, Buffer).


% Reads each node of a map and builds JSON String
consume_obj(Object, Depth, Buffer) when map_size(Object) > 1 ->
    [{Key, Value} | Tail] = maps:to_list(Object),
    consume_obj(maps:from_list(Tail), Depth, consume_node(Key, Value, Depth, Buffer));

consume_obj(Object, Depth, Buffer) ->
    [{Key, Value}] = maps:to_list(Object),
    consume_node(Key, Value, Depth, Buffer).


% Consumes a given node and performs different string manipulations based on type
consume_node(Key, Value, Depth, Buffer) when is_map(Value) ->
    case maps:size(Value) > 0 of
        true ->
            AfterOpenBrace = format("\"~s\": {~n", [Key], Depth, Buffer),
            AfterNodeContents = consume_obj(Value, Depth + 1, AfterOpenBrace),
            AfterCloseBrace = format("},~n", [], Depth, AfterNodeContents),
            AfterCloseBrace;
        false ->
            format("\"~s\": {}~n", [Key], Depth, Buffer)
    end;

consume_node(Key, Value, Depth, Buffer) when is_list(Value) ->
    case length(Value) > 0 of
        true ->
            AfterOpenBrace = format("\"~s\": [~n", [Key], Depth, Buffer),
            AfterNodeContents = consume_arr(Value, Depth + 1, AfterOpenBrace),
            AfterCloseBrace = format("],~n", [], Depth, AfterNodeContents),
            AfterCloseBrace;
        false ->
            format("\"~s\": []~n", [Key], Depth, Buffer)
    end;

consume_node(Key, Value, Depth, Buffer) when ?IS_NUMBER(Value) ; ?IS_ALLOWED_ATOM(Value) ->
    format("\"~s\": ~w,~n", [Key, Value], Depth, Buffer);

consume_node(Key, Value, Depth, Buffer) when is_bitstring(Value) ->
    format("\"~s\": \"~s\",~n", [Key, bitstring_to_list(Value)], Depth, Buffer);

consume_node(Key, Value, Depth, Buffer) ->
    format("\"~s\": \"~w\", // Unsupported datatype converted to string to try and not break JSON~n", [Key, Value], Depth, Buffer).


% Wrapper around io_lib:format that takes into account a depth for automated indentation
format(String, Vars, 0, Buffer) ->
    % io:format(String, Vars),
    NewBuffer = io_lib:format("~s" ++ String, [Buffer] ++ Vars),
    NewBuffer;
format(String, Vars, Depth, Buffer) ->
    % io:format((lists:flatten(lists:map(fun(_) -> "\t" end, lists:seq(1, Depth)))) ++ String, Vars),
    NewBuffer = io_lib:format("~s" ++ (lists:flatten(lists:map(fun(_) -> "\t" end, lists:seq(1, Depth)))) ++ String, [Buffer] ++ Vars),
    NewBuffer.
