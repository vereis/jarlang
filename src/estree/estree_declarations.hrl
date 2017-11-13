% Generates a variable declaration
variableDeclaration(Declarations, Kind)  ->
    ?spec([{Declarations, list_of_variableDeclarator}, {Kind, [variableType]}]),
    updateRecord(declaration(), [{"type", <<"VariableDeclaration">>}, {"declarations", Declarations}, {"kind", Kind}]).

variableDeclaration_test() ->
    ?assertEqual(variableDeclaration([variableDeclarator(identifier("a"), literal(15))], <<"const">>), #{
        "declarations" => [
            #{
                "id" => #{
                    "name" => "a",
                    "type" => <<"Identifier">>
                },
                "init" => #{
                    "type" => <<"Literal">>,
                    "value" => 15
                },
                "type" => <<"VariableDeclarator">>
            }
        ],
        "kind" => <<"const">>,
        "type" => <<"VariableDeclaration">>
    }).

is_variableDeclaration(?NODETYPE(<<"VariableDeclaration">>)) ->
    true;
is_variableDeclaration(_) ->
    false.

% Generates a variable declarator
variableDeclarator(Identifier, Init) ->
    ?spec([{Identifier, identifier}, {Init, expression}]),
    node("VariableDeclarator", [{"id", Identifier}, {"init", Init}]).

variableDeclarator_test() ->
    ?assertEqual(variableDeclarator(identifier("a"), literal(15)), #{
        "id" => #{
            "name" => "a",
            "type" => <<"Identifier">>
        },
        "init" => #{
            "type" => <<"Literal">>,
            "value" => 15
        },
        "type" => <<"VariableDeclarator">>
    }).

is_variableDeclarator(?NODETYPE(<<"VariableDeclarator">>)) ->
    true;
is_variableDeclarator(_) ->
    false.

% Generate a function declaration
functionDeclaration(Identifier, Params, Body) ->
    ?spec([{Identifier, identifier}, {Params, list_of_identifier}, {Body, [expression, blockStatement]}]),
    updateRecord(declaration(), [{"type", <<"FunctionDeclaration">>}, {"id", Identifier}, {"params", Params}, {"body", Body}]).

functionDeclaration_test() ->
    ?assertEqual(functionDeclaration(identifier("a"), [], blockStatement([emptyStatement()])), #{
        "body" => #{
            "body" => [
                #{
                    "type" => <<"EmptyStatement">>
                }
            ],
            "type" => <<"BlockStatement">>
        },
        "id" => #{
            "name" => "a",
            "type" => <<"Identifier">>
        },
        "params" => [],
        "type" => <<"FunctionDeclaration">>
    }).

is_functionDeclaration(?NODETYPE(<<"FunctionDeclaration">>)) ->
    true;
is_functionDeclaration(_) ->
    false.