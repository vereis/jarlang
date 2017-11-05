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
blockStatement(Body) ->
    case {is_list(Body)} of
        {true} ->
            updateRecord(statement(), [{"type", <<"BlockStatement">>}, {"body", Body}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [list], [typeof(Body)])
    end.

is_blockStatement(?NODETYPE(<<"BlockStatement">>)) ->
    true;
is_blockStatement(_) ->
    false.

% Generates an if statement
ifStatement(Test, Consequent, Alternate) ->
    case {is_expressionStatement(Test)} of
        {true} ->    
            updateRecord(statement(), [{"type", <<"IfStatement">>}, {"test", Test}, {"consequent", Consequent}, {"alternate", Alternate}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"ExpressionStatement">>, <<"Statement">>, <<"Statement">>], [nodetype(Test), nodetype(Consequent), nodetype(Alternate)])
    end.

is_ifStatement(?NODETYPE(<<"IfStatement">>)) ->
    true;
is_ifStatement(_) ->
    false.

% Generates a Labeled statement (prefixed with break/continue)
labeledStatement(Identifier, Body) ->
    case {is_identifier(Identifier)} of
        {true} ->
            updateRecord(statement(), [{"type", <<"LabeledStatement">>}, {"label", Identifier}, {"body", Body}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Identifier">>, <<"Statement">>], [nodetype(Identifier), nodetype(Body)])
    end.

is_labeledStatement(?NODETYPE(<<"LabeledStatement">>)) ->
    true;
is_labeledStatement(_) ->
    false.

% Generates a break statement
breakStatement(Identifier) ->
    case {is_identifier(Identifier), Identifier =:= null} of
        {true, _} ->
            updateRecord(statement(), [{"type", <<"BreakStatement">>}, {"label", Identifier}]);
        {_, true} ->
            updateRecord(statement(), [{"type", <<"BreakStatement">>}, {"label", Identifier}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Identifier">>], [nodetype(Identifier)])
    end.

is_breakStatement(?NODETYPE(<<"BreakStatement">>)) ->
    true;
is_breakStatement(_) ->
    false.

% Generates a continue statement
continueStatement(Identifier) ->
    case {is_identifier(Identifier), Identifier =:= null} of
        {true, _} ->
            updateRecord(statement(), [{"type", <<"ContinueStatement">>}, {"label", Identifier}]);
        {_, true} ->
            updateRecord(statement(), [{"type", <<"ContinueStatement">>}, {"label", Identifier}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Identifier">>], [nodetype(Identifier)])
    end.

is_continueStatement(?NODETYPE(<<"ContinueStatement">>)) ->
    true;
is_continueStatement(_) ->
    false.    

% Generates a with statement
withStatement(Expression, BlockStatement) ->
    case {is_blockStatement(BlockStatement)} of
        {true} ->
            updateRecord(statement(), [{"type", <<"WithStatement">>}, {"object", Expression}, {"body", BlockStatement}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Expression">>, <<"BlockStatement">>], [nodetype(Expression), nodetype(BlockStatement)])
    end.

is_withStatement(?NODETYPE(<<"WithStatement">>)) ->
    true;
is_withStatement(_) ->
    false.

% Generates a switch statement
switchStatement(Expression, Cases, HasLexScope) ->
    case {is_list(Cases), lists:all(fun(Case) -> is_switchCase(Case) end, Cases), is_boolean(HasLexScope)} of
        {true, true, true} ->
            updateRecord(statement(), [{"type", <<"SwitchStatement">>}, {"discriminant", Expression}, {"cases", Cases}, {"lexical", HasLexScope}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Expression">>, <<"List<SwitchCase>">>, boolean], 
                                       [nodetype(Expression), {typeof(Cases), lists:map(fun(Case) -> nodetype(Case) end, Cases)}, typeof(HasLexScope)])
    end.

is_switchStatement(?NODETYPE(<<"SwitchStatement">>)) ->
    true;
is_switchStatement(_) ->
    false.

switchCase(Test, Consequent) ->
    case {is_list(Consequent)} of
        {true} ->
            updateRecord(statement(), [{"type", <<"SwitchCase">>}, {"test", Test}, {"consequent", Consequent}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Expression">>, <<"List<Statement>">>], [nodetype(Test), 
                                                          {typeof(Consequent), lists:map(fun(X) -> nodetype(X) end, Consequent)}])
    end.

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
% Note, type checking this one is a little difficult
tryStatement(BlockStatement, Handler, GuardedHandler, Finalizer) ->
    case {is_blockStatement(BlockStatement), is_blockStatement(Finalizer)} of
        {true, true} ->
            updateRecord(statement(), [{"type", <<"TryStatement">>}, {"block", BlockStatement}, {"handler", Handler}, {"guardedHandler", GuardedHandler}, {"finalizer", Finalizer}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"BlockStatement">>, <<"CatchClause">>, <<"List<CatchClause">>, <<"BlockStatement">>], 
                                       [nodetype(BlockStatement), nodetype(Handler), {typeof(GuardedHandler), lists:map(fun(X) -> nodetype(X) end, GuardedHandler)}, nodetype(Finalizer)])
    end.

is_tryStatement(?NODETYPE(<<"TryStatement">>)) ->
    true;
is_tryStatement(_) ->
    false.

catchClause(Param, Guard, Body) ->
    case {is_blockStatement(Body)} of
        {true} ->
            updateRecord(statement(), [{"type", <<"CatchClause">>}, {"param", Param}, {"guard", Guard}, {"body", Body}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Pattern(?)">>, <<"Expression">>, <<"BlockStatement">>], [typeof(Param), nodetype(Guard), nodetype(Body)])
    end.

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
    case {lists:any(fun(X) -> X =:= true end, [is_variableDeclaration(Init), Init =:= null])} of
        {true} ->
            updateRecord(whileStatement(Expression, Body), [{"type", <<"ForStatement">>}, {"init", Init}, {"update", Update}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"VariableDeclaration or null">>, <<"Expression or null">>, <<"Expression">>, <<"Statement">>], 
                                       [nodetype(Init), nodetype(Update), nodetype(Expression), nodetype(Body)])
    end.

is_forStatement(?NODETYPE(<<"ForStatement">>)) ->
    true;
is_forStatement(_) ->
    false.

% Generates a forIn statement
forInStatement(Left, Right, Body, Each) ->
    case {is_variableDeclaration(Left)} of
        {true} ->
            updateRecord(statement(), [{"type", <<"ForInStatement">>}, {"left", Left}, {"right", Right}, {"each", Each}, {"body", Body}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"VariableDeclaration or Expression">>, <<"Expression">>, <<"Statement">>, boolean], 
                                       [nodetype(Left), nodetype(Right), nodetype(Body), typeof(Each)])
    end.

is_forInStatement(?NODETYPE(<<"ForInStatement">>)) ->
    true;
is_forInStatement(_) ->
    false.

% Generates a forOf statement
forOfStatement(Left, Right, Body) ->
    case {is_variableDeclaration(Left)} of
        {true} ->
            updateRecord(statement(), [{"type", <<"ForOfStatement">>}, {"left", Left}, {"right", Right}, {"body", Body}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"VariableDeclaration or Expression">>, <<"Expression">>, <<"Statement">>], 
                                       [nodetype(Left), nodetype(Right), nodetype(Body)])
    end.

is_forOfStatement(?NODETYPE(<<"ForOfStatement">>)) ->
    true;
is_forOfStatement(_) ->
    false.