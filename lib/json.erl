-module(json).
-compile(export_all).

obj() ->
    {err, no_object}.

obj(Object) when is_map(Object) ->
   print("{~n", [], 0),
   obj(Object, 1),
   print("}~n", [], 0).

obj(Object, Depth) when is_map(Object), map_size(Object) > 1 ->
    [{Key, Value} | Tail] = maps:to_list(Object),
    consumeNode(Key, Value, Depth),
    obj(maps:from_list(Tail), Depth);
obj(Object, Depth) when is_map(Object) ->
    [{Key, Value}] = maps:to_list(Object),
    consumeNode(Key, Value, Depth).

consumeNode(Key, Value, Depth) when is_map(Value) ->
    print("\"~s\": {~n", [Key], Depth),
    obj(Value, Depth+1),
    print("},~n", [], Depth);
consumeNode(Key, Value, Depth) when is_list(Value) ->
    print("\"~s\": ~w,~n", [Key, Value], Depth); % TODO: iterate through value and print strings as strings
consumeNode(Key, Value, Depth) when is_integer(Value); is_float(Value) ->
    print("\"~s\": ~w,~n", [Key, Value], Depth);
consumeNode(Key, Value, Depth) when is_bitstring(Value) ->
    print("\"~s\": \"~s\",~n", [Key, bitstring_to_list(Value)], Depth);
consumeNode(Key, Value, Depth) ->
    print("\"~s\": \"~w\", // Unsupported datatype converted to string to try and not break JSON~n", [Key, Value], Depth).

print(String, Vars, 0) ->
    io:format(String, Vars);
print(String, Vars, Depth) ->
    io:format((lists:flatten(lists:map(fun(_) -> "\t" end, lists:seq(1, Depth)))) ++ String, Vars).
