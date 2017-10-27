% Throw an error string
error(Type, Description, Message) ->
    throwStatement(
        binaryExpression(<<"+">>, 
            literal(iolist_to_binary(["** ", Type, ": ", Description])),
            Message
        )
    ).

% Use strict directive
useStrict() ->
    expressionStatement(literal(<<"use_strict">>)).

% Variable declarator shorthand
constDeclaration(Identifier, Init) ->
    variableDeclaration(variableDeclarator(identifier(Identifier), Init), <<"const">>).

varDeclaration(Identifier, Init) ->
    variableDeclaration(variableDeclarator(identifier(Identifier), Init), <<"var">>).

letDeclaration(Identifier, Init) ->
    variableDeclaration(variableDeclarator(identifier(Identifier), Init), <<"let">>).