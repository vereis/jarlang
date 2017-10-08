% Erlang to JavaScript transpiler
-module(jrlscript).
-export([compile/1]).

compile(Path) ->
    compile:file("./src/coregen.erl"),
    code:add_path("./src/"),
    coregen:er2ast(Path).