% Generates an empty statement node - i.e. an empty semicolon
emptyStatement() ->
    updateRecord(statement(), [{"type", <<"EmptyStatement">>}]).

is_emptyStatement(?NODETYPE(<<"EmptyStatement">>)) ->
    true;
is_emptyStatement(_) ->
    false.

% Generates an Expression Statement
expressionStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ExpressionStatement">>}, {"expression", Expression}]).

is_expressionStatement(?NODETYPE(<<"ExpressionStatement">>)) ->
    true;
is_expressionStatement(_) ->
    false.

% Generates a block statement
blockStatement(Body) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"BlockStatement">>}, {"body", Body}]);
blockStatement(Body) ->
    blockStatement([Body]).

is_blockStatement(?NODETYPE(<<"BlockStatement">>)) ->
    true;
is_blockStatement(_) ->
    false.

% Generates an if statement
ifStatement(Test, Consequent, Alternate) ->
    updateRecord(statement(), [{"type", <<"IfStatement">>}, {"test", Test}, {"consequent", Consequent}, {"alternate", Alternate}]).

is_ifStatement(?NODETYPE(<<"IfStatement">>)) ->
    true;
is_ifStatement(_) ->
    false.

% Generates a Labeled statement (prefixed with break/continue)
labeledStatement(Identifier, Body) ->
    updateRecord(statement(), [{"type", <<"LabeledStatement">>}, {"label", Identifier}, {"body", Body}]).

is_labeledStatement(?NODETYPE(<<"LabeledStatement">>)) ->
    true;
is_labeledStatement(_) ->
    false.

% Generates a break statement
breakStatement(Identifier) ->
    updateRecord(statement(), [{"type", <<"BreakStatement">>}, {"label", Identifier}]).

is_breakStatement(?NODETYPE(<<"BreakStatement">>)) ->
    true;
is_breakStatement(_) ->
    false.

% Generates a continue statement
continueStatement(Identifier) ->
    updateRecord(statement(), [{"type", <<"ContinueStatement">>}, {"label", Identifier}]).

is_continueStatement(?NODETYPE(<<"ContinueStatement">>)) ->
    true;
is_continueStatement(_) ->
    false.    

% Generates a with statement
withStatement(Expression, Statement) ->
    updateRecord(statement(), [{"type", <<"WithStatement">>}, {"object", Expression}, {"body", Statement}]).

is_withStatement(?NODETYPE(<<"WithStatement">>)) ->
    true;
is_withStatement(_) ->
    false.

% Generates a switch statement
switchStatement(Expression, Cases, HasLexScope) when is_list(Cases) ->
    updateRecord(statement(), [{"type", <<"SwitchStatement">>}, {"discriminant", Expression}, {"cases", Cases}, {"lexical", HasLexScope}]);
switchStatement(Expression, Cases, HasLexScope) ->
    switchStatement(Expression, [Cases], HasLexScope).

is_switchStatement(?NODETYPE(<<"SwitchStatement">>)) ->
    true;
is_switchStatement(_) ->
    false.

switchCase(Test, Consequent) when is_list(Consequent) ->
   updateRecord(statement(), [{"type", <<"SwitchCase">>}, {"test", Test}, {"consequent", Consequent}]);
switchCase(Test, Consequent) ->
    switchCase(Test, [Consequent]).

is_switchCase(?NODETYPE(<<"SwitchCase">>)) ->
    true;
is_switchCase(_) ->
    false.

% Generates a return statement
returnStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ReturnStatement">>}, {"argument", Expression}]).

is_returnStatement(?NODETYPE(<<"ReturnStatement">>)) ->
    true;
is_returnStatement(_) ->
    false.

% Generates a throw statement
throwStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ThrowStatement">>}, {"argument", Expression}]).

is_throwStatement(?NODETYPE(<<"ThrowStatement">>)) ->
    true;
is_throwStatement(_) ->
    false.

% Generates a try statement
tryStatement(BlockStatement, Handler, GuardedHandler, Finalizer) ->
    updateRecord(statement(), [{"type", <<"TryStatement">>}, {"block", BlockStatement}, {"handler", Handler}, {"guardedHandler", GuardedHandler}, {"finalizer", Finalizer}]).

is_tryStatement(?NODETYPE(<<"TryStatement">>)) ->
    true;
is_tryStatement(_) ->
    false.

catchClause(Param, Guard, Body) ->
    updateRecord(statement(), [{"type", <<"CatchClause">>}, {"param", Param}, {"guard", Guard}, {"body", Body}]).

is_catchClause(?NODETYPE(<<"CatchClause">>)) ->
    true;
is_catchClause(_) ->
    false.

% Generates a while statement
whileStatement(Expression, Body) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"WhileStatement">>}, {"test", Expression}, {"body", Body}]);
whileStatement(Expression, Body) ->
    whileStatement(Expression, [Body]).

is_whileStatement(?NODETYPE(<<"WhileStatement">>)) ->
    true;
is_whileStatement(_) ->
    false.

% Generates a for statement
forStatement(Init, Update, Expression, Body) ->
    updateRecord(whileStatement(Expression, Body), [{"type", <<"ForStatement">>}, {"init", Init}, {"update", Update}]).

is_forStatement(?NODETYPE(<<"ForStatement">>)) ->
    true;
is_forStatement(_) ->
    false.

% Generates a forIn statement
forInStatement(Left, Right, Body, Each) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"ForInStatement">>}, {"left", Left}, {"right", Right}, {"each", Each}, {"body", Body}]);
forInStatement(Left, Right, Body, Each) ->
    forInStatement(Left, Right, [Body], Each).

is_forInStatement(?NODETYPE(<<"ForInStatement">>)) ->
    true;
is_forInStatement(_) ->
    false.

% Generates a forOf statement
forOfStatement(Left, Right, Body) when is_list(Body) ->
    updateRecord(statement(), [{"type", <<"ForOfStatement">>}, {"left", Left}, {"right", Right}, {"body", Body}]);
forOfStatement(Left, Right, Body) ->
    forOfStatement(Left, Right, [Body]).

is_forOfStatement(?NODETYPE(<<"ForOfStatement">>)) ->
    true;
is_forOfStatement(_) ->
    false.