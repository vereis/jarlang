-module(esast).
-compile(export_all). 
-compile({no_auto_import, [node/0]}).

% -- EStree definitions --
-include("estree_primitives.hrl").
-include("estree_statements.hrl").
-include("estree_declarations.hrl").
-include("estree_expressions.hrl").

% -- Functions --
c_module(ModuleName, _Contents) ->
    program(
        variableDeclaration(
            variableDeclarator(
                identifier(ModuleName),
                callExpression(
                    functionExpression(
                        null,
                        [],
                        blockStatement(
                            []
                        ),
                        false
                    ),
                    []
                )
            ),
            const
        )
    ).


% ------ Intenal ------ %

% Generates a plain node, setting only node type
node(Type) ->
    node(Type, []).

% Generates a plain node and sets additional fields in the form
% List[{Key, Value}]
node() ->
    #{}.

node(Type, AdditionalFields) ->
    NewNode = #{type => list_to_binary(Type)},
    updateRecord(NewNode, AdditionalFields).


% Helper function which appends new key value pairs into an existing record
updateRecord(Record, [{Key, Value}]) ->
    Record#{Key => Value};
updateRecord(Record, [{Key, Value} | Tail]) ->
    updateRecord(Record#{Key => Value}, Tail).


% Misc Functions
print(Json) ->
    code:add_path("../lib/"),
    io:format(json:serialize(Json)).
