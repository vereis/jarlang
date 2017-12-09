%% Erlang implementation of the MDN documentation for the
%% JavaScript AST which you can find here: https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API
%%
%% Note, JS AST functions are named the same as the equivalent builder function in
%% Mozilla's Spidermonkey JavaScript parser and thus are written in camelcase rather than
%% the more idiomatic allman case. Non-node generating functions are cased normally as
%% per erlang idioms.
%%
%% eunit tests are name the same as the function they're testing, plus _test as eunit
%% dictates. Thus you get a weird combination of allman case and camelcase for nodes
%% such as binaryExpression_test(...)
-author(["Chris Bailey"]).

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

is_variableType_test() ->
    ?assertEqual(is_variableType(<<"var">>), true),
    ?assertEqual(is_variableType(<<"let">>), true),
    ?assertEqual(is_variableType(<<"const">>), true),
    ?assertEqual(is_variableType(<<"somethingelse">>), false).

is_anything(_) ->
    true.

is_anything_test() ->
    ?assertEqual(is_anything(an_atom), true),
    ?assertEqual(is_anything("a list string"), true),
    ?assertEqual(is_anything(12), true),
    ?assertEqual(is_anything(0.14), true),
    ?assertEqual(is_anything(<<"bit string">>), true),
    ?assertEqual(is_anything(#{}), true).

is_null(X) ->
    X =:= null.

is_null_test() ->
    ?assertEqual(is_null(null), true),
    ?assertEqual(is_null(an_atom), false),
    ?assertEqual(is_null("null"), false),
    ?assertEqual(is_null(12), false),
    ?assertEqual(is_null(0.14), false),
    ?assertEqual(is_null(<<"null">>), false),
    ?assertEqual(is_null(#{}), false).

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

node_test() ->
    ?assertEqual(node(), #{}),
    ?assertEqual(node(atomname, []), #{"type" => <<"atomname">>}),
    ?assertEqual(node("listname", []), #{"type" => <<"listname">>}),
    ?assertEqual(node(random, [{1, 2}]), #{"type" => <<"random">>, 1 => 2}).

% Add location data to any node
addLocationData(Node, LineNumber, ColStart, ColEnd) ->
    updateRecord(Node, [{"loc", sourceLocation(LineNumber, ColStart, ColEnd)}]).

addLocationData_test() ->
    ?assertEqual(
        addLocationData(
            blockStatement([emptyStatement(), emptyStatement(), emptyStatement()]),
            12, 15, 22
        ),
        #{
            "body" => [
                #{"type" => <<"EmptyStatement">>},
                #{"type" => <<"EmptyStatement">>},
                #{"type" => <<"EmptyStatement">>}
            ],
            "loc" => #{
                "end" => #{
                    "column" => 22,
                    "line" => 12,
                    "type" => <<"Position">>
                },
                "source" => null,
                "start" => #{
                    "column" => 15,
                    "line" => 12,
                    "type" => <<"Position">>
                },
                "type" => <<"SourceLocation">>
            },
            "type" => <<"BlockStatement">>
        }
    ).

% Helper function which appends new key value pairs into an existing record
updateRecord(Record, []) ->
    Record;
updateRecord(Record, [{Key, Value}]) ->
    Record#{Key => Value};
updateRecord(Record, [{Key, Value} | Tail]) ->
    updateRecord(Record#{Key => Value}, Tail).

updateRecord_test() ->
    ?assertEqual(updateRecord(#{a => 1, b => 2, c => 3}, [{4, 5}]), #{a => 1, b => 2, c => 3, 4 => 5}),
    ?assertEqual(updateRecord(#{a => 1, b => 2, c => 3}, [{<<"string">>, 5}]), #{a => 1, b => 2, c => 3, <<"string">> => 5}).