-module(esast).
-compile(export_all).

module(ModuleName, Contents) ->
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

node(Type, Data) ->
    Node = io_lib:format("~s", [node(Type, Data, ["{\n", "\"type\": ", Type, ",\n"])]),
    lists:flatten(erlang:iolist_to_binary(Node)).

node(Type, [], Node) ->
    Node ++ "\n}";
node(Type, [{Key, Value} | Tails], Node) ->
    node(Type, Tails, Node ++ "\"" ++ Key ++ "\": " ++ Value ++ ",\n").

