-module(esast).
-compile(export_all). 
-compile({no_auto_import, [node/0]}).

test() -> false.

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

% Generates a Labeled statement (prefixed with break/continue)
labeledStatement(Identifier, Body) ->
    updateRecord(statement(), [{"type", <<"LabeledStatement">>}, {"label", Identifier}, {"body", Body}]).

% Generates a break statement
breakStatement(Identifier) ->
    updateRecord(statement(), [{"type", <<"BreakStatement">>}, {"label", Identifier}]).

% Generates a continue statement
continueStatement(Identifier) ->
    updateRecord(statement(), [{"type", <<"ContinueStatement">>}, {"label", Identifier}]).

% Generates a with statement
withStatement(Expression, Statement) ->
    updateRecord(statement(), [{"type", <<"WithStatement">>}, {"object", Expression}, {"body", Statement}]).

% Generates a switch statement
switchStatement(Expression, Cases, HasLexScope) when is_list(Cases) ->
    updateRecord(statement(), [{"type", <<"SwitchStatement">>}, {"discriminant", Expression}, {"cases", Cases}, {"lexical", HasLexScope}]);
switchStatement(Expression, Cases, HasLexScope) ->
    switchStatement(Expression, [Cases], HasLexScope).

% Generates a return statement
returnStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ReturnStatement">>}, {"argument", Expression}]).

% Generates a throw statement
throwStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ThrowStatement">>}, {"argument", Expression}]).

% Generates a try statement
tryStatement(BlockStatement, Handler, GuardedHandler, Finalizer) ->
    updateRecord(statement(), [{"type", <<"TryStatement">>}, {"block", BlockStatement}, {"handler", Handler}, {"guardedHandler", GuardedHandler}, {"finalizer", Finalizer}]).

% Generates a while statement
whileStatement(Expression, Body) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"WhileStatement">>}, {"test", Expression}, {"body", Body}]);
whileStatement(Expression, Body) ->
    whileStatement(Expression, [Body]).

% Generates a for statement
forStatement(Init, Update, Expression, Body) ->
    updateRecord(whileStatement(Expression, Body), [{"type", <<"ForStatement">>}, {"init", Init}, {"update", Update}]).

% Generates a forIn statement
forInStatement(Left, Right, Body) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"ForInStatement">>}, {"left", Left}, {"right", Right}, {"each", false}, {"body", Body}]);
forInStatement(Left, Right, Body) ->
    forInStatement(Left, Right, [Body]).


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
    io:format(json:serialize(Json)).
