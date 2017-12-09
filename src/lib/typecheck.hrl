%% Run-time type checking library which is disabled unless the attribute -debug(true) exists within
%% the compiled module where this is used.
%% Also provides a useful typeof(X) function, alongside a useful is_bif(X) function
-author(["Chris Bailey"]).

-ifdef(debug).
% Returns a tuple that looks like {CurrentFunctionName, Arity}
-define(CURRENT_FUNCTION,
    {
        element(2, element(2, process_info(self(), current_function))), 
        element(3, element(2, process_info(self(), current_function)))
    }).

% ?spec takes a list of tuples in the form of [{Arg, type}] where Arg is a reference to a
% parameter, and type is a atom stating what type Arg should be.
% Valid types are things such as atom, boolean, list.
% If a given type is not a built in type, ?spec naively tries to check the type of a given 
% Arg by calling ?MODULE:is_customtype(Arg) where customtype is the type given.
% ?spec supports one additional level for compound types allowing types such as list_of_type,
% where again, type can be a built-in type or a compound type.
-define(spec(TypeList),
    __TypecheckTests = lists:map(fun({Arg, Type}) -> 
        case is_list(Type) of
            true ->
                {Arg, Type, lists:map(fun(T) -> list_to_atom("is_" ++ atom_to_list(T)) end, Type)};
            _ ->
                {Arg, Type, [list_to_atom("is_" ++ atom_to_list(Type))]}
        end
    end, TypeList),
    
    case lists:all(fun(X) -> X =:= true end, lists:map(fun({Arg, _, TypeFns}) -> 
        lists:member(true, lists:map(fun(TypeFn) -> 
            case is_bif(TypeFn) of
                true ->
                    erlang:TypeFn(Arg);
                _ ->
                    case is_compound_type(TypeFn) of
                        true ->
                            [OuterType , _] = re:replace(atom_to_list(TypeFn), "_of_.*$", ""),
                            [_ | InnerType] = re:replace(atom_to_list(TypeFn), "^is_(list|tuple)_of_", ""),
                            OuterTypeFn = list_to_atom(binary_to_list(OuterType)),
                            InnerTypeFn = list_to_atom("is_" ++ binary_to_list(InnerType)),    
                            case erlang:OuterTypeFn(Arg) of
                                true ->
                                    lists:all(fun(X) -> X =:= true end, 
                                        lists:map(fun(X) -> 
                                            case is_bif(InnerTypeFn) of
                                                true ->
                                                    erlang:InnerTypeFn(X);
                                                _ ->
                                                    ?MODULE:InnerTypeFn(X)
                                            end
                                        end, Arg)
                                    );
                                _ ->
                                    false
                            end;
                        _ ->
                            ?MODULE:TypeFn(Arg)
                    end
            end
        end, TypeFns))
    end, __TypecheckTests)) of 
        false ->
            throw(
                badArgs(?CURRENT_FUNCTION, 
                    lists:map(fun({_, Type, _}) -> Type end, __TypecheckTests), 
                    lists:map(fun({Arg, _, _})  -> typeof(Arg) end, __TypecheckTests),
                    lists:map(fun({Arg, _, _})  -> Arg end, __TypecheckTests)
                )
            );
        _ ->
            ok
    end).
-else.
-define(spec(X), true).
-endif.

badArgs(Function, Expected, Got, As) ->
    error({badargs, Function, {expected, Expected}, {got, Got}, {as, As}}).

% Implementing typeof for built in types
typeof(X) when is_pid(X) ->
    pid;
typeof(X) when is_port(X) ->
    port;
typeof(X) when is_reference(X) ->
    reference;
typeof(X) when is_list(X) ->
    list;
typeof(X) when is_boolean(X) ->
    boolean;
typeof(X) when is_atom(X) ->
    atom;
typeof(X) when is_bitstring(X) ->
    bitstring;
typeof(X) when is_float(X) ->
    float;
typeof(X) when is_integer(X) ->
    integer;
typeof(#{"type" := Type}) ->    
    list_to_atom(string:to_lower(binary_to_list(Type)));
typeof(X) when is_map(X) ->
    map;
typeof(X) when is_tuple(X) ->
    tuple;
typeof(X) ->
    {err, unknown_type, X}.

is_bif(X) ->
    case re:run(atom_to_list(X), "^(is_boolean|is_pid|is_port|is_reference|is_list|is_atom|is_bitstring|is_float|is_integer|is_map|is_tuple)$") of
        {match, _} ->
            true;
        _ ->
            false
    end.

is_compound_type(X) ->
    case re:run(atom_to_list(X), "^(is_(list|tuple)_of_)") of
        {match, _} ->
            true;
        _ ->
            false
    end.
