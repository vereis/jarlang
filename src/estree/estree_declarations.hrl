% Generates a variable declaration
variableDeclaration(Declarations, Kind)  ->
    case {is_list(Declarations), 
          lists:all(fun(X) -> X =:= true end, lists:map(fun(X) -> is_variableDeclarator(X) end, Declarations)), 
          lists:member(Kind, [<<"var">>, <<"let">>, <<"const">>])} of
        {true, true, true} ->
            updateRecord(declaration(), [{"type", <<"VariableDeclaration">>}, {"declarations", Declarations}, {"kind", Kind}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"List<VariableDeclarator>">>, <<"var, let or const">>], 
                                       [{typeof(Declarations), lists:map(fun(X) -> nodetype(X) end, Declarations)}, Kind])
    end.

is_variableDeclaration(?NODETYPE(<<"VariableDeclaration">>)) ->
    true;
is_variableDeclaration(_) ->
    false.

% Generates a variable declarator
variableDeclarator(Identifier, Init) ->
    case {is_identifier(Identifier), is_expression(Init)} of
        {true, true} ->
            node("VariableDeclarator", [{"id", Identifier}, {"init", Init}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Identifier">>, <<"Expression">>], [nodetype(Identifier), nodetype(Init)])
    end.

is_variableDeclarator(?NODETYPE(<<"VariableDeclarator">>)) ->
    true;
is_variableDeclarator(_) ->
    false.

% Generate a function declaration
functionDeclaration(Identifier, Params, Body) ->
    case {is_identifier(Identifier), is_blockStatement(Body)} of
        {true, true} ->
            updateRecord(declaration(), [{"type", <<"FunctionDeclaration">>}, {"id", Identifier}, {"params", Params}, {"body", Body}]);
        _ ->
            badArgs(?CURRENT_FUNCTION, [<<"Identifier">>, <<"List<Pattern(?)>">>, <<"BlockStatement or Expression">>], 
                                       [nodetype(Identifier), nodetype(Params), nodetype(Body)])
    end.

    
is_functionDeclaration(?NODETYPE(<<"FunctionDeclaration">>)) ->
    true;
is_functionDeclaration(_) ->
    false.