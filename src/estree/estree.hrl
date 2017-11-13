% Macros for use in guards in certain places
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
 
% Include typechecking header file containing a macro ?spec.
% ?spec takes a list of tuples in the form of [{Arg, type}] where Arg is a reference to a
% parameter, and type is a atom stating what type Arg should be.
% Valid types are things such as atom, boolean, list.
% If a given type is not a built in type, ?spec naively tries to check the type of a given 
% Arg by calling ?MODULE:is_customtype(Arg) where customtype is the type given.
% ?spec supports one additional level for compound types allowing types such as list_of_type,
% where again, type can be a built-in type or a compound type.
-include("../lib/typecheck.hrl").

% Custom type declarations  
is_variableType(X) ->
    lists:member(X, [<<"var">>, <<"let">>, <<"const">>]).

is_anything(_) ->
    true.

is_null(X) ->
    X =:= null.

% Include ESTREE Declarations, which we seperate into types for readability
% All nodetypes defined in these header files also have custom type declarations for use with
% typecheck.hrl 
-include("estree_primitives.hrl").
-include("estree_statements.hrl").
-include("estree_declarations.hrl").
-include("estree_expressions.hrl").
-include("estree_prefabs.hrl").

% Functions

% Generates a plain node, setting only node type
node(Type) ->
    node(Type, []).

% Generates a plain node and sets additional fields in the form
% List[{Key, Value}]
node() ->
    #{}.

node(Type, AdditionalFields) when is_atom(Type) ->
    node(atom_to_binary(Type, utf8), AdditionalFields);
node(Type, AdditionalFields) when is_list(Type) ->
    node(list_to_binary(Type), AdditionalFields);
node(Type, AdditionalFields) ->
    NewNode = #{"type" => Type},
    updateRecord(NewNode, AdditionalFields).

% Add location data to any node
addLocationData(Node, LineNumber, ColStart, ColEnd) ->
    updateRecord(Node, [{"loc", sourceLocation(LineNumber, ColStart, ColEnd)}]).

% Helper function which appends new key value pairs into an existing record
updateRecord(Record, [{Key, Value}]) ->
    Record#{Key => Value};
updateRecord(Record, [{Key, Value} | Tail]) ->
    updateRecord(Record#{Key => Value}, Tail).
