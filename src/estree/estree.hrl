-define(IS_LITERAL(X),
    is_list(X) ; is_binary(X) ; X =:= null ; X =:= undefined ; is_integer(X) ; is_float(X) ; is_boolean(X)).
        
-define(IS_IDENTIFIER(X),
    is_list(X) ; is_binary(X)).

-define(IS_UNARY_OPERATOR(X), 
    X =:= <<"-">> ;      X =:= <<"+">> ;    X =:= <<"!">> ; X =:= <<"~">> ; 
    X =:= <<"typeof">> ; X =:= <<"void">> ; X =:= <<"delete">>).

-define(IS_BINARY_OPERATOR(X),
    X =:= <<"==">> ; X =:= <<"!=">> ; X =:= <<"===">> ; X =:= <<"!==">> ;
    X =:= <<"<">>  ; X =:= <<">">>  ; X =:= <<"<=">>  ; X =:= <<">=">> ;
    X =:= <<"<<">> ; X =:= <<">>">> ; X =:= <<">>>">> ; X =:= <<"+">> ;
    X =:= <<"-">>  ; X =:= <<"*">>  ; X =:= <<"/">>   ; X =:= <<"%">> ;
    X =:= <<"|">>  ; X =:= <<"^">>  ; X =:= <<"&">>   ; X =:= <<"in">> ;
    X =:= <<"instanceof">> ; X =:= <<"..">>).

-define(IS_LOGICAL_OPERATOR(X),
    X =:= <<"||">> ; X =:= <<"&&">>).

-define(IS_ASSIGNMENT_OPERATOR(X),
    X =:= <<"=">>    ; X =:= <<"+=">> ; X =:= <<"-=">>  ; X =:= <<"*=">> ; 
    X =:= <<"/=">>   ; X =:= <<"%=">> ; X =:= <<"<<=">> ; X =:= <<">>=">> ;
    X =:= <<">>>=">> ; X =:= <<"|=">> ; X =:= <<"^=">>  ; X =:= <<"&=">>).

-define(IS_UPDATE_OPERATOR(X),
    X =:= <<"++">> ; X =:= <<"--">>).

-define(NODETYPE(X),
    #{"type" := X}).

-define(CURRENT_FUNCTION,
    {
        element(2, element(2, process_info(self(), current_function))), 
        element(3, element(2, process_info(self(), current_function)))
    }).

-define(spec(TypeList),
    __TypecheckTests = lists:map(fun({Arg, Type}) -> 
        case is_list(Type) of
            true ->
                {Arg, Type, lists:map(fun(T) -> list_to_atom("is_" ++ atom_to_list(T)) end, Type)};
            _ ->
                {Arg, Type, [list_to_atom("is_" ++ atom_to_list(Type))]}
        end
    end, TypeList),
    
    % Actually does all the checks we need. 
    % For BIFs, we just run erlang:Fn(Arg), otherwise we need to determine if a compound type is in play.
    % If so, we ensure the argument is a list and check everything inside the list is of the expected type.
    % If not, we just check an is_TYPE function defined in the current module.
    % All typecheck functions are stored in a list and thus, we are able to have ANY of the tests within the
    % list pass
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
                    lists:map(fun({Arg, _, _})  -> nodetype(Arg) end, __TypecheckTests),
                    lists:map(fun({Arg, _, _})  -> Arg end, __TypecheckTests)
                )
            );
        _ ->
            ok
    end).

-include("estree_primitives.hrl").
-include("estree_statements.hrl").
-include("estree_declarations.hrl").
-include("estree_expressions.hrl").
-include("estree_prefabs.hrl").

badArgs(Function, Expected, Got) ->
    badArgs(Function, Expected, Got, []).

badArgs(Function, Expected, Got, As) ->
    error({badargs, Function, {expected, Expected}, {got, Got}, {as, As}}).

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
typeof(X) when is_map(X) ->
    map;
typeof(X) when is_tuple(X) ->
    tuple;
typeof(X) ->
    {err, unknown_type, X}.

is_null(X) ->
    X =:= null.

is_anything(_) ->
    true.

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

is_variableType(X) ->
    lists:member(X, [<<"var">>, <<"let">>, <<"const">>]).
    
nodetype(#{"type" := Type}) ->
    list_to_atom(string:to_lower(binary_to_list(Type)));
nodetype(Var) ->
    typeof(Var).