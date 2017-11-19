% Author: Andrew Johnson
% Walks down a CoreErlang AST and calls EStree.

-module(asttrans).
-export([erast2esast/1]).

% Compiles a given Erlang source file to a EStree ast
erast2esast(AST) ->
    parseModule(AST).

%Read the module token (first token)
parseModule({c_module, _A, {_, _, ModuleName}, Exports, _Attributes, Functions})->
    io:format("module: ~s ~n", [ModuleName]),
    io:format("    exports: ~p ~n", [tupleList_getVars_3(Exports)]),
    FormattedFunctions = parseFunctions(Functions),
    FormattedExports = lists:map(fun({N,A})->{atom_to_list(N),A} end,tupleList_getVars_3(Exports)),
    %io:format("~p~n~n~n~n~n~n", [FormattedFunctions]),
    esast:c_module(atom_to_list(ModuleName),FormattedExports,FormattedFunctions);
    
    
parseModule(T)->
    io:format("Unrecognised Token in module section: ~p", [T]).


%Split functions apart
parseFunctions(Functions)->
    lists:map(fun(X)->parseFunction(X) end,Functions).

%Read a function
parseFunction({{_, _, {FunctionName, Arity}}, {_, [compiler_generated], _, _}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(null,[],esast:blockStatement([esast:emptyStatement()]),false)};
parseFunction({{_, _, {FunctionName, Arity}}, {_, _, ParamNames, Body}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(
        null,
        tupleListToIdentifierList(ParamNames),
        esast:blockStatement(
            encapsulateExpressions(
                listCheck(
                    parseFunctionBody(return,Body)
                )
            )
        ),
        false
    )}.

encapsulateExpressions(L)->
    lists:map(
        fun(X)->
            IsStmt = esast:is_statement(X),
            if
                IsStmt->X;
                true->esast:expressionStatement(X)
            end
        end,
        L
    ).

listCheck([L]) when is_list(L) ->
    listCheck(L);
listCheck(L)->
    IsStmt = esast:is_statement(L),
    if
        IsStmt->[L];
        true->L
    end.


%Parse the function body

parseFunctionBody(return,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Params})->
    esast:returnStatement(parseFunctionBody(noreturn,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Params}));



%Handle Operator calls



parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '+'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"addition">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '-'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"subtraction">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '*'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"multiplication">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '/'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"division">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'rem'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"remainder">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'div'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"intDivision">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );



parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '=='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"equality">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '/='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"notEquality">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '<'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"lessThan">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '=<'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"lessThanOrEq">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '>'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"moreThan">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '>='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"moreThanOrEq">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '=:='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"exactlyEq">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, '=/='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"exactlyNotEq">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );




parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'or'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"or">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'and'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"and">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'not'}, [T]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"not">>),false),
         [parseFunctionBody(noreturn,T)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'xor'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"xor">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );



parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'band'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"band">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'bor'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"bor">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'bxor'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"bxor">>),false),
         [parseFunctionBody(noreturn,L),parseFunctionBody(noreturn,R)]
     );
parseFunctionBody(noreturn,{c_call, _, {_, _, erlang}, {_, _, 'bnot'}, [T]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"bnot">>),false),
         [parseFunctionBody(noreturn,T)]
     );



%Bodge job: external module calls
%parseFunctionBody(noreturn,{c_call, _, {_, _, io}, {_, _, format}, [T]})->
%    esast:callExpression(
%         esast:memberExpression(esast:identifier(<<"console">>),esast:identifier(<<"log">>),false),
%         [parseFunctionBody(noreturn,T)]
%     );


parseFunctionBody(noreturn,{c_call, _, {_, _, Module}, {_, _, FunctionName}, Params})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(atom_to_binary(Module,utf8)),esast:identifier(atom_to_binary(FunctionName,utf8)),false),
         lists:map(fun(T)->parseFunctionBody(noreturn,T) end,Params)
     );


parseFunctionBody(return,{c_values, _, _Values})->
    %io:format("~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);

parseFunctionBody(return,{c_var, A, Var})->
    esast:returnStatement(parseFunctionBody(noreturn,{c_var, A, Var}));
parseFunctionBody(noreturn,{c_var, _, Var})->
    esast:identifier(atom_to_binary(Var,utf8));
    %io:format("",[]);


parseFunctionBody(return,{c_seq, _, A, B})->
    assembleSequence(
        parseFunctionBody(noreturn,A),
        parseFunctionBody(return,B));
parseFunctionBody(noreturn,{c_seq, _, A, B})->
    assembleSequence(
        parseFunctionBody(noreturn,A),
        parseFunctionBody(noreturn,B));



parseFunctionBody(return,{c_let, _, [{_, _, _Variable}], _A, _B})->
    %io:format("        Let statement: ~s~n", [Variable]),
    %parseFunctionBody(A),
    %parseFunctionBody(B);
    io:format("",[]);
    
% Is apply a local function call? Assignment from function? Assignment with pattern matching?
parseFunctionBody(return,{c_apply, _, {_,_,{_FName,_Arity}}, _Params})->
    %io:format("Call local function ~s(",[FName]),
    %io:format("~p)~n", [tupleList_getVars_3(Params)]);
    io:format("",[]);

parseFunctionBody(return,{c_apply, _, _A, _B})->
    %io:format("        Apply statement: ~n");
    io:format("",[]);
    
parseFunctionBody(return,{c_literal,_,Value})->
    esast:returnStatement(parseFunctionBody(noreturn,{c_literal,[],Value}));
parseFunctionBody(noreturn,{c_literal,_,Value}) when is_number(Value)->
    esast:newExpression(esast:identifier(<<"ErlNumber">>),esast:literal(Value));
parseFunctionBody(noreturn,{c_literal,_,Value}) when is_atom(Value)->
    esast:newExpression(esast:identifier(<<"Atom">>),esast:literal(atom_to_list(Value)));
parseFunctionBody(noreturn,{c_literal,_,Value}) when is_list(Value)->
    esast:newExpression(esast:identifier(<<"List">>),esast:literal(Value));

parseFunctionBody(return,{c_tuple,_,_Values})->
    %io:format("        Tuple ~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);
    
parseFunctionBody(return,{c_primop,_,{_,_,Type},_Details})->
    % io:format("        Error? ~p~n~p~n", [Type,Details]),
    esast:error(atom_to_list(Type),"TODO Errors dont parse nicely",esast:literal("Message"));
    % io:format("",[]);


parseFunctionBody(ReturnAtom,{c_case, _, {c_var,_,Var}, Clauses})->
    parseFunctionBody(ReturnAtom,{c_case, a, {c_values,a,[{c_var,a,Var}]}, Clauses});

parseFunctionBody(ReturnAtom,{c_case, _, {c_values,_,Vars}, Clauses})->
    parseCaseClauses(ReturnAtom, Vars, Clauses);


parseFunctionBody(_,T)->
    io:format("Unrecognised Token in function body: ~p", [T]).



parseCaseClauses(ReturnAtom, Vars, [])->
    null;
parseCaseClauses(ReturnAtom, Vars, [{c_clause,_,Match,Evaluate,Consequent}|Clauses])->
    esast:ifStatement(
        assembleCaseCondition(Vars,Match,Evaluate),%test
        esast:blockStatement(%consequent
            encapsulateExpressions(
                listCheck(
                    parseFunctionBody(ReturnAtom,Consequent)
                )
            )
        ),
        parseCaseClauses(ReturnAtom, Vars, Clauses)%alternate
    ).

assembleCaseCondition(_,[],Evaluate)->
    parseFunctionBody(noreturn,Evaluate);
assembleCaseCondition(Vars,Match,{c_literal,_,true})->
    assembleCaseCondition(Vars,Match);
assembleCaseCondition(Vars,Match,Evaluate)->
    esast:logicalExpression(<<"&&">>,assembleCaseCondition(Vars,Match),parseFunctionBody(noreturn,Evaluate)).

assembleCaseCondition([V],[M])->
        parseFunctionBody(noreturn,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [M,V]});
assembleCaseCondition([V|Vars],[M|Match])->
    esast:logicalExpression(<<"&&">>,
        assembleCaseCondition([V],[M]),
        assembleCaseCondition(Vars,Match)
    ).


assembleSequence(L,R) when is_list(L) and is_list(R)->
    lists:append(L,R);
assembleSequence(L,R) when is_list(R)->
    [L|R];
assembleSequence(L,R) when is_list(L)->
    lists:append(L,[R]);
assembleSequence(L,R)->
    [L,R].


tupleListToIdentifierList(List)->
    lists:map(fun({c_var,[],A})->parseFunctionBody(noreturn,{c_var,[],A}) end,List).



tupleList_getVars_3([])->
    [];
tupleList_getVars_3([{_,_, Val} | Body])->
    [Val | tupleList_getVars_3(Body)];
tupleList_getVars_3([{_, _, Val, _} | Body])->
    [Val | tupleList_getVars_3(Body)].

%rAtomToList([A|Rest])->
%    [rAtomToList(A)|rAtomToList(Rest)];
%rAtomToList({A})->
%    {rAtomToList(A)};
%rAtomToList({A,B})->
%    {rAtomToList(A),rAtomToList(B)};
%rAtomToList({A,B,C})->
%    {rAtomToList(A),rAtomToList(B),rAtomToList(C)};
%rAtomToList({A,B,C,D})->
%    {rAtomToList(A),rAtomToList(B),rAtomToList(C),rAtomToList(D)};
%rAtomToList(A) when is_atom(A) ->
%    atom_to_list(A);
%rAtomToList(A) when not is_atom(A) ->
%    A.









% Currently unused so commenting out for compilation
%tuple_to_string(T) ->
%   lists:flatten(io:format("~p", [T])).
