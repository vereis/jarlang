% Author: Andrew Johnson
% Utilities to make working in this folder easier

-module(revEng_Util).
-export([estree/0,
		 remake_files/0]).

estree()->
	code:add_path("../../lib/"),
	code:add_path("../../edebug/"),
	io:format("~p~n",[jarlang:er2(estree,"./core_AST_revEng")]).

remake_files()->
	code:add_path("../../lib/"),
	code:add_path("../../edebug/"),
	jarlang:er2file(core_erlang,"./core_AST_revEng"),
	jarlang:er2file(core_AST,"./core_AST_revEng").