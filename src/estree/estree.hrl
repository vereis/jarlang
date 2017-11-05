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

-include("estree_primitives.hrl").
-include("estree_statements.hrl").
-include("estree_declarations.hrl").
-include("estree_expressions.hrl").
-include("estree_prefabs.hrl").

badArgs(Function, Expected, Got) ->
    error({badargs, Function, {expected, Expected}, {got, Got}}).

typeof(X) when is_pid(X) ->
    pid;
typeof(X) when is_port(X) ->
    port;
typeof(X) when is_reference(X) ->
    reference;
typeof(X) when is_list(X) ->
    list;
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

nodetype(#{"type" := Type}) ->
    Type;
nodetype(Var) ->
    {err, no_node_given, Var}.