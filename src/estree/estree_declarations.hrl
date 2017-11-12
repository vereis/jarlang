% Generates a variable declaration
variableDeclaration(Declarations, Kind)  ->
    ?spec([{Declarations, list_of_variableDeclarator}, {Kind, [variableType]}]),
    updateRecord(declaration(), [{"type", <<"VariableDeclaration">>}, {"declarations", Declarations}, {"kind", Kind}]).

is_variableDeclaration(?NODETYPE(<<"VariableDeclaration">>)) ->
    true;
is_variableDeclaration(_) ->
    false.

% Generates a variable declarator
variableDeclarator(Identifier, Init) ->
    ?spec([{Identifier, identifier}, {Init, expression}]),
    node("VariableDeclarator", [{"id", Identifier}, {"init", Init}]).

is_variableDeclarator(?NODETYPE(<<"VariableDeclarator">>)) ->
    true;
is_variableDeclarator(_) ->
    false.

% Generate a function declaration
functionDeclaration(Identifier, Params, Body) ->
    ?spec([{Identifier, identifier}, {Params, list_of_identifier}, {Body, [expression, blockStatement]}]),
    updateRecord(declaration(), [{"type", <<"FunctionDeclaration">>}, {"id", Identifier}, {"params", Params}, {"body", Body}]).
    
is_functionDeclaration(?NODETYPE(<<"FunctionDeclaration">>)) ->
    true;
is_functionDeclaration(_) ->
    false.