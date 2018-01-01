% Author: Andrew Johnson
% Walks down a CoreErlang AST and calls EStree.
-module(asttrans).
-export([erast2esast/1]).

% Compiles a given Erlang source file to a EStree ast
erast2esast(AST) ->
    parseModule(AST).

%Read the module token (first token)
parseModule({c_module, _A, {_, _, ModuleName}, Exports, _Attributes, Functions})->
    FormattedFunctions = parseFunctions(Functions),
    FormattedExports = lists:map(fun({N,A})->{atom_to_list(N),A} end,tupleList_getVars_3(Exports)),
    esast:c_module(atom_to_list(ModuleName),FormattedExports,FormattedFunctions);

parseModule(T)->
    io:format("Unrecognised Token in module section: ~p", [T]).

% Concurrently transpile core erlang asts for functions into javascript asts for functions
parseFunctions(Functions)->
    Self = self(),
    Pids = lists:map(fun(X) ->
        spawn_link(fun() -> Self ! {self(), parseFunction(X)} end)
    end, Functions),

    [
        receive
            {Pid, TranspiledFunction} ->
                TranspiledFunction
        end
        ||
        Pid <- Pids
    ].

%Read a function
parseFunction({{_, _, {FunctionName, Arity}}, {_c_fun, [compiler_generated], _, _}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(null,[],esast:blockStatement([esast:emptyStatement()]),false)};
parseFunction({{_, _, {FunctionName, Arity}}, {_c_fun, _, ParamNames, Body}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),esast:functionExpression(
        null,
        tupleListToIdentifierList(ParamNames,tupleList_getVars_3(ParamNames)),
        esast:blockStatement(
            encapsulateExpressions(
                listCheck(
                    parseFunctionBody(return,tupleList_getVars_3(ParamNames),Body)
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

parseFunctionBody(return,Params,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters})->
    esast:returnStatement(parseFunctionBody(noreturn,Params,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters}));



%Handle Operator calls



parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '+'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"addition">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '-'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"subtraction">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '*'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"multiplication">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '/'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"division">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'rem'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"remainder">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'div'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"intDivision">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );



parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '=='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"equality">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '/='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"notEquality">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '<'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"lessThan">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '=<'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"lessThanOrEq">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '>'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"moreThan">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '>='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"moreThanOrEq">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '=:='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"exactlyEq">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '=/='}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"exactlyNotEq">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );




parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'or'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"or">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'and'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"and">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'not'}, [T]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"not">>),false),
         [parseFunctionBody(noreturn,Params,T)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'xor'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"xor">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );



parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'band'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"band">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'bor'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"bor">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'bxor'}, [L,R]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"bxor">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'bnot'}, [T]})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(<<"erlang">>),esast:identifier(<<"bnot">>),false),
         [parseFunctionBody(noreturn,Params,T)]
     );



%Bodge job: external module calls
%parseFunctionBody(noreturn,Params,{c_call, _, {_, _, io}, {_, _, format}, [T]})->
%    esast:callExpression(
%         esast:memberExpression(esast:identifier(<<"console">>),esast:identifier(<<"log">>),false),
%         [parseFunctionBody(noreturn,Params,T)]
%     );


parseFunctionBody(noreturn,Params,{c_call, _, {_, _, Module}, {_, _, FunctionName}, Parameters})->
    esast:callExpression(
         esast:memberExpression(esast:identifier(atom_to_binary(Module,utf8)),esast:identifier(atom_to_binary(FunctionName,utf8)),false),
         lists:map(fun(T)->parseFunctionBody(noreturn,Parameters,T) end,Parameters)
     );


parseFunctionBody(return,Params,{c_values, _, _Values})->
    %io:format("~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);

parseFunctionBody(return,Params,{c_var, A, Var})->
    esast:returnStatement(parseFunctionBody(noreturn,Params,{c_var, A, Var}));
parseFunctionBody(noreturn,Params,{c_var, _, Var})->
    esast:identifier(atom_to_binary(Var,utf8));
    %io:format("",[]);


parseFunctionBody(return,Params,{c_seq, _, A, B})->
    assembleSequence(
        parseFunctionBody(noreturn,Params,A),
        parseFunctionBody(return,Params,B));
parseFunctionBody(noreturn,Params,{c_seq, _, A, B})->
    assembleSequence(
        parseFunctionBody(noreturn,Params,A),
        parseFunctionBody(noreturn,Params,B));


% A let statement is the core representation of implicit variable declarations (the result of some function as an argument of another function
parseFunctionBody(ReturnAtom,Params,{c_let, _, [{_, _, Variable}], Value, UsedBy})->
    assembleSequence(
        esast:variableDeclaration([esast:variableDeclarator(esast:identifier(atom_to_binary(Variable,utf8)),parseFunctionBody(noreturn,Params,Value))],<<"let">>),
        parseFunctionBody(ReturnAtom,Params,UsedBy));

% Is apply a local function call? Assignment from function? Assignment with pattern matching?
parseFunctionBody(ReturnAtom,Params,{c_apply, _, {_,_,{FunctionName,Arity}}, Parameters})->
    parseFunctionBody(ReturnAtom,Params,{c_call, [], {a, a, exports}, {a, a, FunctionName}, Parameters});

parseFunctionBody(ReturnAtom,Params,{c_apply, _, {_, _, FunctionName}, Parameters})->
    parseFunctionBody(ReturnAtom,Params,{c_call, [], {a, a, exports}, {a, a, FunctionName}, Parameters});

parseFunctionBody(return,Params,{c_literal,_,Value})->
    esast:returnStatement(parseFunctionBody(noreturn,Params,{c_literal,[],Value}));
parseFunctionBody(noreturn,Params,{c_literal,_,Value}) when is_number(Value)->
    esast:newExpression(esast:identifier(<<"ErlNumber">>),[esast:literal(Value)]);
parseFunctionBody(noreturn,Params,{c_literal,_,Value}) when is_atom(Value)->
    esast:newExpression(esast:identifier(<<"Atom">>),[esast:literal(atom_to_binary(Value, utf8))]);
parseFunctionBody(noreturn,Params,{c_literal,_,Value}) when is_list(Value)->
    esast:newExpression(esast:identifier(<<"List">>),[esast:literal(Value)]);

parseFunctionBody(return,Params,{c_tuple,_,_Values})->
    %io:format("        Tuple ~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);




%List constructor
parseFunctionBody(return,Params,{c_cons,_,A,B})->
    esast:returnStatement(parseFunctionBody(noreturn,Params,{c_cons,[],A,B}));

parseFunctionBody(noreturn,Params,{c_cons,_,A,B={c_cons,_,C,D}})->
    esast:newExpression(esast:identifier(<<"List">>),
        parseConsChain(noreturn,Params,{c_cons,[],A,B})
   );

parseFunctionBody(noreturn,Params,{c_cons,_,A,B})->
    esast:newExpression(esast:identifier(<<"List">>),[parseFunctionBody(noreturn,Params,A),parseFunctionBody(noreturn,Params,B)]);



parseFunctionBody(return,Params,{c_primop,_,{_,_,Type},_Details})->
    % io:format("        Error? ~p~n~p~n", [Type,Details]),
    esast:error(atom_to_list(Type),"TODO Errors dont parse nicely\\n",esast:literal(<<"Message">>));
    % io:format("",[]);


parseFunctionBody(ReturnAtom,Params,{c_case, _, {c_var,_,Var}, Clauses})->
    parseFunctionBody(ReturnAtom,Params,{c_case, a, {c_values,a,[{c_var,a,Var}]}, Clauses});

parseFunctionBody(ReturnAtom,Params,{c_case, _, {c_values,_,Vars}, Clauses})->
    {UnboundVars,CaseClauses} = parseCaseClauses(ReturnAtom,Params, Vars, Clauses),
    case UnboundVars of
        [] -> CaseClauses;
        _  -> assembleSequence(esast:variableDeclaration(UnboundVars,<<"let">>),CaseClauses)
    end;

parseFunctionBody(_,Params,T)->
    io:format("Unrecognised Token in function body: ~p~n", [T]).



parseConsChain(noreturn,Params,{c_cons,[],A,B={c_cons,_,C,D}})->
    [parseFunctionBody(noreturn,Params,A)|parseConsChain(noreturn,Params,B)];
parseConsChain(noreturn,Params,{c_cons,[],A,B})->
    [parseFunctionBody(noreturn,Params,A)].





parseCaseClauses(ReturnAtom,Params, Vars, [])->
    {[],[]};
parseCaseClauses(ReturnAtom,Params, Vars, [{c_clause,_,Match,Evaluate,Consequent}|Clauses])->
    {UnboundVars,ElseClauses} = parseCaseClauses(ReturnAtom,Params, Vars, Clauses),%alternate
    case ElseClauses of
        [] -> ElseClausesActual = null;
        _  -> ElseClausesActual = ElseClauses
    end,
    {lists:append(declaratorsFromList(Match),UnboundVars),
     esast:ifStatement(
        assembleCaseCondition(Params,Vars,Match,Evaluate),%test
        esast:blockStatement(%consequent
            assembleSequence(
                lists:filter(fun(Elem)->
                        case Elem of
                            ok -> false;
                            _  -> true
                        end
                    end,
                    assignMatchedVars(Params,Vars,Match)
                ),
                encapsulateExpressions(
                    listCheck(
                        parseFunctionBody(ReturnAtom,Params,Consequent)
                    )
                )
            )
        ),
        ElseClausesActual %alternate
    )}.

assembleCaseCondition(Params,_,[],Evaluate)->
    parseFunctionBody(noreturn,Params,Evaluate);
assembleCaseCondition(Params,Vars,Match,{c_literal,_,true})->
    assembleCaseCondition(Params,Vars,Match);
assembleCaseCondition(Params,Vars,Match,Evaluate)->
    esast:logicalExpression(<<"&&">>,assembleCaseCondition(Params,Vars,Match),parseFunctionBody(noreturn,Params,Evaluate)).

assembleCaseCondition(Params,[V],[M])->
        parseFunctionBody(noreturn,Params,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [M,V]});
assembleCaseCondition(Params,[V|Vars],[M|Match])->
    esast:logicalExpression(<<"&&">>,
        assembleCaseCondition(Params,[V],[M]),
        assembleCaseCondition(Params,Vars,Match)
    ).

assignMatchedVars(Params,[V],[M])->
        assignMatchedVars(Params,V,M);
assignMatchedVars(Params,[V|Vars],[M|Match])->
    assembleSequence(
        assignMatchedVars(Params,[V],[M]),
        assignMatchedVars(Params,Vars,Match)
    );
assignMatchedVars(Params,{c_var,_,Variable},{c_var,_,Match})->
    [esast:expressionStatement(
        esast:assignmentExpression(
            <<"=">>,
            esast:identifier(atom_to_binary(Match,utf8)),
            esast:identifier(atom_to_binary(Variable,utf8))
        )
    )];
assignMatchedVars(Params,_,_)->
    [ok].


assembleSequence(L,R) when is_list(L) and is_list(R)->
    lists:append(L,R);
assembleSequence(L,R) when is_list(R)->
    [L|R];
assembleSequence(L,R) when is_list(L)->
    lists:append(L,[R]);
assembleSequence(L,R)->
    [L,R].


tupleListToIdentifierList(List,Params)->
    lists:map(fun({c_var,[],A})->parseFunctionBody(noreturn,Params,{c_var,[],A}) end,List).



tupleList_getVars_3([])->
    [];
tupleList_getVars_3([{_,_, Val} | Body])->
    [Val | tupleList_getVars_3(Body)];
tupleList_getVars_3([{_, _, Val, _} | Body])->
    [Val | tupleList_getVars_3(Body)].


declaratorsFromList(List)->
    lists:filtermap(fun(Elem)->
        case Elem of
            {c_var,_,Name} -> {true,esast:variableDeclarator(esast:identifier(atom_to_binary(Name,utf8)),esast:identifier(<<"undefined">>))};
            _              -> false
        end
    end,List).


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
