-module(esast).
-compile(export_all). 
-compile({no_auto_import, [node/0]}).

module(ModuleName, _Contents) ->
    io_lib:format(
"{
    \"type\": \"Program\",
    \"body\": [
        {
            \"type\": \"VariableDeclaration\",
            \"declarations\": [
                {
                    \"type\": \"VariableDeclarator\",
                    \"id\": {
                        \"type\": \"Identifier\",
                        \"name\": \"~s\"
                    },
                    \"init\": {
                        \"type\": \"CallExpression\",
                        \"callee\": {
                            \"type\": \"FunctionExpression\",
                            \"id\": null,
                            \"params\": [],
                            \"body\": {
                                \"type\": \"BlockStatement\",
                                \"body\": [],
                            },
                            \"generator\": false,
                            \"expression\": false,
                        },
                        \"arguments\": []
                    }
                }
            ],
            \"kind\": \"const\"
        }
    ],
    \"sourceType\": \"module\"
}~n", [ModuleName]).

% Generates a plain node, setting only node type
node(Type) ->
    node(Type, []).

% Generates an Identifier Node
identifier(Name) when is_binary(Name) ->
    node("Identifier", [{"name", Name}]).

% Generates a Literal Node
literal(Value) when is_binary(Value) ; Value =:= null ; is_integer(Value) ; is_float(Value) ; is_boolean(Value) -> % Need to add regex definition
    node("Literal", [{"value", Value}]).

% Generates a RegExp Literal
regexLiteral(Pattern, Flags) ->
    updateRecord(literal(null), [{regex, #{pattern => Pattern, flags => Flags}}]).

% Generates a Program Node
program(Statement) ->
    node("Program", [{"body", Statement}]).

% Generates a Function Node
% Note: This doesn't seem to be used directly, but instead via being modified to become 
% a functionDeclaration node or a functionExpression node
function(Identifier, Params, Body) when is_list(Body) ->
    node("function", [{"id", Identifier}, {"params", Params}, {"body", Body}]);
function(Identifier, Params, Body) ->
    function(Identifier, Params, [Body]).

% Generates a generic Statement Node
statement() ->
    node().

% Generates an empty statement node - i.e. an empty semicolon
emptyStatement() ->
    updateRecord(statement(), [{"type", <<"EmptyStatement">>}]).

% Generates an Expression Statement
expressionStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ExpressionStatement">>}, {"expression", Expression}]).

% Generates a block statement
blockStatement(Body) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"BlockStatement">>}, {"body", Body}]);
blockStatement(Body) ->
    blockStatement([Body]).

% Generates an if statement
ifStatement(Test, Consequent, Alternate) ->
    updateRecord(statement(), [{"type", <<"IfStatement">>}, {"test", Test}, {"consequent", Consequent}, {"alternate", Alternate}]).

% ------ Intenal ------ %

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
    io:format(json:toJson(Json)).
