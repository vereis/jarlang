% Author: Chris Bailey & Andrew Johnson
% Transpiles erlang to javascript

-module(jarlang).
-export([er2/2,
		 er2/3]).

% Transpiles a module through the pipeline as far as is implemented
er2(best,Module) ->
	er2(core_erlang,Module),
	AST=er2(core_AST,Module),
	asttrans:erast2esast(AST);


er2(core_erlang,Module)->
	coregen:er2ce(Module);


er2(core_AST,Module)->
	{ok,AST}=coregen:er2ast(Module,return_AST),
	AST.

er2(core_AST,Module,to_file)->
	coregen:er2ast(Module).