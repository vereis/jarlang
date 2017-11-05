-define(IS_LITERAL(X),
        is_list(X) ; is_binary(X) ; X =:= null ; X =:= undefined ; is_integer(X) ; is_float(X) ; is_boolean(X)).
        
-define(IS_IDENTIFIER(X),
	is_list(X) ; is_binary(X)).

-define(IS_UNARY_OPERATOR(X), 
        X =:= <<"-">> ;      X =:= <<"+">> ;    X =:= <<"!">> ; X =:= <<"~">> ; 
        X =:= <<"typeof">> ; X =:= <<"void">> ; X =:= <<"delete">>).

-define(IS_BINARY_OPERATOR(X),
        X =:= <<"==">> ; X =:= <<"!=">> ; X =:= <<"===">> ; X =:= <<"!==">> ;
        X =:= <<"<">>  ; X =:= <<">">>  ; X =:= <<"<=">>  ; X =:= <<">=">> ;
        X =:= <<"<<">> ; X =:= <<">>">> ; X =:= <<">>>">> ; X =:= <<"+">> ;
        X =:= <<"-">>  ; X =:= <<"*">>  ; X =:= <<"/">>   ; X =:= <<"%">> ;
        X =:= <<"|">>  ; X =:= <<"^">>  ; X =:= <<"&">>   ; X =:= <<"in">> ;
        X =:= <<"instanceof">> ; X =:= <<"..">>).

-define(IS_LOGICAL_OPERATOR(X),
        X =:= <<"||">> ; X =:= <<"&&">>).

-define(IS_ASSIGNMENT_OPERATOR(X),
        X =:= <<"=">>    ; X =:= <<"+=">> ; X =:= <<"-=">>  ; X =:= <<"*=">> ; 
        X =:= <<"/=">>   ; X =:= <<"%=">> ; X =:= <<"<<=">> ; X =:= <<">>=">> ;
        X =:= <<">>>=">> ; X =:= <<"|=">> ; X =:= <<"^=">>  ; X =:= <<"&=">>).

-define(IS_UPDATE_OPERATOR(X),
        X =:= <<"++">> ; X =:= <<"--">>).

-define(NODETYPE(X),
    #{type := X}).

-include("estree_primitives.hrl").
-include("estree_statements.hrl").
-include("estree_declarations.hrl").
-include("estree_expressions.hrl").
-include("estree_prefabs.hrl").