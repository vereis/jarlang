% Generates an empty statement node - i.e. an empty semicolon
emptyStatement() ->
    updateRecord(statement(), [{"type", <<"EmptyStatement">>}]).

emptyStatement_test() ->
    ?assertEqual(emptyStatement(), #{"type" => <<"EmptyStatement">>}).

is_emptyStatement(?NODETYPE(<<"EmptyStatement">>)) ->
    true;
is_emptyStatement(_) ->
    false.

% Generates an Expression Statement
expressionStatement(Expression) ->
    updateRecord(statement(), [{"type", <<"ExpressionStatement">>}, {"expression", Expression}]).

expressionStatement_test() ->
    ?assertEqual(expressionStatement(thisExpression()), #{
        "type" => <<"ExpressionStatement">>,
        "expression" => #{
            "type" => <<"ThisExpression">>    
        }
    }).

is_expressionStatement(?NODETYPE(<<"ExpressionStatement">>)) ->
    true;
is_expressionStatement(_) ->
    false.

% Generates a block statement
blockStatement(Body) ->
    ?spec([{Body, list_of_statement}]),
    updateRecord(statement(), [{"type", <<"BlockStatement">>}, {"body", Body}]).

blockStatement_test() ->
    ?assertEqual(blockStatement([]), #{"type" => <<"BlockStatement">>, "body" => []}),
    ?assertEqual(blockStatement([emptyStatement()]), #{
        "body" => [#{"type" => <<"EmptyStatement">>}],
        "type" => <<"BlockStatement">>}
    ).

is_blockStatement(?NODETYPE(<<"BlockStatement">>)) ->
    true;
is_blockStatement(_) ->
    false.

% Generates an if statement
ifStatement(Test, Consequent, Alternate) ->
    ?spec([{Test, expression}, {Consequent, statement}, {Alternate, [statement, null]}]),
    updateRecord(statement(), [{"type", <<"IfStatement">>}, {"test", Test}, {"consequent", Consequent}, {"alternate", Alternate}]).

ifStatement_test() ->
    ?assertEqual(ifStatement(thisExpression(), emptyStatement(), null), #{"alternate" => null,
    "consequent" => #{"type" => <<"EmptyStatement">>},
    "test" => #{"type" => <<"ThisExpression">>},
    "type" => <<"IfStatement">>}),
    ?assertEqual(ifStatement(thisExpression(), emptyStatement(), emptyStatement()), #{"alternate" => #{"type" => <<"EmptyStatement">>},
    "consequent" => #{"type" => <<"EmptyStatement">>},
    "test" => #{"type" => <<"ThisExpression">>},
    "type" => <<"IfStatement">>}).

is_ifStatement(?NODETYPE(<<"IfStatement">>)) ->
    true;
is_ifStatement(_) ->
    false.

% Generates a Labeled statement (prefixed with break/continue)
labeledStatement(Identifier, Body) ->
    ?spec([{Identifier, identifier}, {Body, statement}]),
    updateRecord(statement(), [{"type", <<"LabeledStatement">>}, {"label", Identifier}, {"body", Body}]).

labeledStatement_test() ->
    ?assertEqual(labeledStatement(identifier("a"), emptyStatement()), #{"body" => #{"type" => <<"EmptyStatement">>},
    "label" => #{"name" => "a","type" => <<"Identifier">>},
    "type" => <<"LabeledStatement">>}).

is_labeledStatement(?NODETYPE(<<"LabeledStatement">>)) ->
    true;
is_labeledStatement(_) ->
    false.

% Generates a break statement
breakStatement(Identifier) ->
    ?spec([{Identifier, [null, identifier]}]),    
    updateRecord(statement(), [{"type", <<"BreakStatement">>}, {"label", Identifier}]).

breakStatement_test() ->
    ?assertEqual(breakStatement(identifier("a")), #{"label" => #{"name" => "a","type" => <<"Identifier">>},
    "type" => <<"BreakStatement">>}).

is_breakStatement(?NODETYPE(<<"BreakStatement">>)) ->
    true;
is_breakStatement(_) ->
    false.

% Generates a continue statement
continueStatement(Identifier) ->
    ?spec([{Identifier, [null, identifier]}]),    
    updateRecord(statement(), [{"type", <<"ContinueStatement">>}, {"label", Identifier}]).

continueStatement_test() ->
    ?assertEqual(continueStatement(identifier("a")), #{"label" => #{"name" => "a","type" => <<"Identifier">>},
    "type" => <<"ContinueStatement">>}).

is_continueStatement(?NODETYPE(<<"ContinueStatement">>)) ->
    true;
is_continueStatement(_) ->
    false.    

% Generates a with statement
withStatement(Expression, BlockStatement) ->
    ?spec([{Expression, expression}, {BlockStatement, blockStatement}]),
    updateRecord(statement(), [{"type", <<"WithStatement">>}, {"object", Expression}, {"body", BlockStatement}]).

withStatement_test() ->
    ?assertEqual(withStatement(thisExpression(), blockStatement([])), #{"body" => #{"body" => [],"type" => <<"BlockStatement">>},
    "object" => #{"type" => <<"ThisExpression">>},
    "type" => <<"WithStatement">>}).

is_withStatement(?NODETYPE(<<"WithStatement">>)) ->
    true;
is_withStatement(_) ->
    false.

% Generates a switch statement
switchStatement(Expression, Cases, HasLexScope) ->
    ?spec([{Expression, expression}, {Cases, list_of_switchCase}, {HasLexScope, boolean}]),
    updateRecord(statement(), [{"type", <<"SwitchStatement">>}, {"discriminant", Expression}, {"cases", Cases}, {"lexical", HasLexScope}]).

switchStatement_test() ->
    ?assertEqual(switchStatement(thisExpression(), [], false), #{"cases" => [],
    "discriminant" => #{"type" => <<"ThisExpression">>},
    "lexical" => false,
    "type" => <<"SwitchStatement">>}).

is_switchStatement(?NODETYPE(<<"SwitchStatement">>)) ->
    true;
is_switchStatement(_) ->
    false.

switchCase(Test, Consequent) ->
    ?spec([{Test, expression}, {Consequent, list_of_statement}]),
    updateRecord(statement(), [{"type", <<"SwitchCase">>}, {"test", Test}, {"consequent", Consequent}]).

switchCase_test() ->
    ?assertEqual(switchCase(thisExpression(), []), #{"consequent" => [],
    "test" => #{"type" => <<"ThisExpression">>},
    "type" => <<"SwitchCase">>}).

is_switchCase(?NODETYPE(<<"SwitchCase">>)) ->
    true;
is_switchCase(_) ->
    false.

% Generates a return statement
returnStatement(Expression) ->
    ?spec([{Expression, [null, expression]}]),
    updateRecord(statement(), [{"type", <<"ReturnStatement">>}, {"argument", Expression}]).

returnStatement_test() ->
    ?assertEqual(returnStatement(thisExpression()), #{"argument" => #{"type" => <<"ThisExpression">>},
    "type" => <<"ReturnStatement">>}).

is_returnStatement(?NODETYPE(<<"ReturnStatement">>)) ->
    true;
is_returnStatement(_) ->
    false.

% Generates a throw statement
throwStatement(Expression) ->
    ?spec([{Expression, [null, expression]}]),
    updateRecord(statement(), [{"type", <<"ThrowStatement">>}, {"argument", Expression}]).

throwStatement_test() ->
    ?assertEqual(throwStatement(thisExpression()), #{"argument" => #{"type" => <<"ThisExpression">>},
    "type" => <<"ThrowStatement">>}).

is_throwStatement(?NODETYPE(<<"ThrowStatement">>)) ->
    true;
is_throwStatement(_) ->
    false.

% Generates a try statement
% Note, type checking this one is a little difficult
tryStatement(BlockStatement, Handler, GuardedHandler, Finalizer) ->
    ?spec([{BlockStatement, blockStatement},
           {Handler, catchClause},
           {GuardedHandler, list_of_catchClause},
           {Finalizer, blockStatement}]),   
    updateRecord(statement(), [{"type", <<"TryStatement">>}, {"block", BlockStatement}, {"handler", Handler}, 
        {"guardedHandler", GuardedHandler}, {"finalizer", Finalizer}]).

tryStatement_test() ->
    ?assertEqual(tryStatement(blockStatement([]), 
                              catchClause(identifier("a"), thisExpression(), blockStatement([])),
                              [], 
                              blockStatement([])), 
                 #{"block" => #{"body" => [],"type" => <<"BlockStatement">>},
                   "finalizer" => #{"body" => [],"type" => <<"BlockStatement">>},
                   "guardedHandler" => [],
                   "handler" => #{"body" => #{"body" => [],"type" => <<"BlockStatement">>},
                        "guard" => #{"type" => <<"ThisExpression">>},
                        "param" => #{"name" => "a","type" => <<"Identifier">>},
                        "type" => <<"CatchClause">>},
                   "type" => <<"TryStatement">>}).

is_tryStatement(?NODETYPE(<<"TryStatement">>)) ->
    true;
is_tryStatement(_) ->
    false.

catchClause(Param, Guard, Body) ->
    ?spec([{Param, identifier}, {Guard, expression}, {Body, blockStatement}]),
    updateRecord(statement(), [{"type", <<"CatchClause">>}, {"param", Param}, {"guard", Guard}, {"body", Body}]).

catchClause_test() ->
    ?assertEqual(catchClause(identifier("a"), thisExpression(), blockStatement([])), #{
        "body" => #{"body" => [],"type" => <<"BlockStatement">>},
        "guard" => #{"type" => <<"ThisExpression">>},
        "param" => #{"name" => "a","type" => <<"Identifier">>},
        "type" => <<"CatchClause">>}).

is_catchClause(?NODETYPE(<<"CatchClause">>)) ->
    true;
is_catchClause(_) ->
    false.

% Generates a while statement
whileStatement(Expression, Body) ->
    ?spec([{Expression, expression}, {Body, statement}]),    
    updateRecord(statement(), [{"type", <<"WhileStatement">>}, {"test", Expression}, {"body", Body}]).

whileStatement_test() ->
    ?assertEqual(whileStatement(thisExpression(), emptyStatement()), #{"body" => #{"type" => <<"EmptyStatement">>},
    "test" => #{"type" => <<"ThisExpression">>},
    "type" => <<"WhileStatement">>}).

is_whileStatement(?NODETYPE(<<"WhileStatement">>)) ->
    true;
is_whileStatement(_) ->
    false.

% Generates a for statement
forStatement(Init, Update, Expression, Body) ->
    ?spec([{Init, [expression, variableDeclaration, null]},
           {Update, [expression, null]},
           {Expression, expression},
           {Body, statement}]),
    updateRecord(whileStatement(Expression, Body), [{"type", <<"ForStatement">>}, {"init", Init}, {"update", Update}]).

forStatement_test() ->
    ?assertEqual(forStatement(thisExpression(), null, thisExpression(), emptyStatement()), #{
    "body" => #{"type" => <<"EmptyStatement">>},
    "init" => #{"type" => <<"ThisExpression">>},
    "test" => #{"type" => <<"ThisExpression">>},
    "type" => <<"ForStatement">>,
    "update" => null}).

is_forStatement(?NODETYPE(<<"ForStatement">>)) ->
    true;
is_forStatement(_) ->
    false.

% Generates a forIn statement
forInStatement(Left, Right, Body, Each) ->
    ?spec([{Left, [variableDeclaration, expression]}, 
           {Right, expression},
           {Body, statement},
           {Each, boolean}]),
    updateRecord(statement(), [{"type", <<"ForInStatement">>}, {"left", Left}, {"right", Right}, {"each", Each}, {"body", Body}]).

forInStatement_test() ->
    ?assertEqual(forInStatement(thisExpression(), thisExpression(), emptyStatement(), false), #{
    "body" => #{"type" => <<"EmptyStatement">>},
    "each" => false,
    "left" => #{"type" => <<"ThisExpression">>},
    "right" => #{"type" => <<"ThisExpression">>},
    "type" => <<"ForInStatement">>}).

is_forInStatement(?NODETYPE(<<"ForInStatement">>)) ->
    true;
is_forInStatement(_) ->
    false.

% Generates a forOf statement
forOfStatement(Left, Right, Body) ->
    ?spec([{Left, [variableDeclaration, expression]}, {Right, expression}, {Body, statement}]),
    updateRecord(statement(), [{"type", <<"ForOfStatement">>}, {"left", Left}, {"right", Right}, {"body", Body}]).

forOfStatement_test() ->
    ?assertEqual(forOfStatement(thisExpression(), thisExpression(), emptyStatement()), #{
    "body" => #{"type" => <<"EmptyStatement">>},
    "left" => #{"type" => <<"ThisExpression">>},
    "right" => #{"type" => <<"ThisExpression">>},
    "type" => <<"ForOfStatement">>}).

is_forOfStatement(?NODETYPE(<<"ForOfStatement">>)) ->
    true;
is_forOfStatement(_) ->
    false.