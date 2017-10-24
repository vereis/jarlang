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

switchCase(Test, Consequent) when is_list(Consequent) ->
   updateRecord(statement(), [{"type", <<"SwitchCase">>}, {"test", Test}, {"consequent", Consequent}]);
switchCase(Test, Consequent) ->
    switchCase(Test, [Consequent]).

% Generates a return statement
returnStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ReturnStatement">>}, {"argument", Expression}]).

% Generates a throw statement
throwStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ThrowStatement">>}, {"argument", Expression}]).

% Generates a try statement
tryStatement(BlockStatement, Handler, GuardedHandler, Finalizer) ->
    updateRecord(statement(), [{"type", <<"TryStatement">>}, {"block", BlockStatement}, {"handler", Handler}, {"guardedHandler", GuardedHandler}, {"finalizer", Finalizer}]).

catchClause(Param, Guard, Body) ->
    updateRecord(statement(), [{"type", <<"CatchClause">>}, {"param", Param}, {"guard", Guard}, {"body", Body}]).

% Generates a while statement
whileStatement(Expression, Body) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"WhileStatement">>}, {"test", Expression}, {"body", Body}]);
whileStatement(Expression, Body) ->
    whileStatement(Expression, [Body]).

% Generates a for statement
forStatement(Init, Update, Expression, Body) ->
    updateRecord(whileStatement(Expression, Body), [{"type", <<"ForStatement">>}, {"init", Init}, {"update", Update}]).

% Generates a forIn statement
forInStatement(Left, Right, Body, Each) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"ForInStatement">>}, {"left", Left}, {"right", Right}, {"each", Each}, {"body", Body}]);
forInStatement(Left, Right, Body, Each) ->
    forInStatement(Left, Right, [Body], Each).

% Generates a forOf statement
forOfStatement(Left, Right, Body) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"ForOfStatement">>}, {"left", Left}, {"right", Right}, {"body", Body}]);
forOfStatement(Left, Right, Body) ->
    forOfStatement(Left, Right, [Body]).