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
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),estree:functionExpression(null,[],estree:blockStatement([estree:emptyStatement()]),false)};
parseFunction({{_, _, {FunctionName, Arity}}, {_c_fun, _, ParamNames, Body}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),estree:functionExpression(
        null,
        tupleListToIdentifierList(ParamNames,tupleList_getVars_3(ParamNames)),
        estree:blockStatement(
            encapsulateExpressions(
                listCheck(
                    parseFunctionBody(return,tupleList_getVars_3(ParamNames),Body)
                )
            )
        ),
        false
    )}.

is_statement({_, Type, _}) ->
    case re:run(atom_to_list(Type), "Statement|Declaration") of
        {match, _} ->
            true;
        _ ->
            false
    end;
is_statement(_) ->
    false.

encapsulateExpressions(L)->
    lists:map(
        fun(X)->
            IsStmt = is_statement(X),
            if
                IsStmt->X;
                true->estree:expressionStatement(X)
            end
        end,
        L
    ).

listCheck([L]) when is_list(L) ->
    listCheck(L);
listCheck(L)->
    IsStmt = is_statement(L),
    if
        IsStmt->[L];
        true->L
    end.


%Parse the function body

parseFunctionBody(return,Params,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters})->
    estree:returnStatement(parseFunctionBody(noreturn,Params,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters}));



%Handle Operator calls



parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '+'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"addition">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '-'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"subtraction">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '*'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"multiplication">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '/'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"division">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'rem'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"remainder">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'div'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"intDivision">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );



parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '=='}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"equality">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '/='}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"notEquality">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '<'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"lessThan">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '=<'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"lessThanOrEq">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '>'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"moreThan">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '>='}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"moreThanOrEq">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '=:='}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"exactlyEq">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, '=/='}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"exactlyNotEq">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );




parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'or'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"or">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'and'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"and">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'not'}, [T]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"not">>),false),
         [parseFunctionBody(noreturn,Params,T)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'xor'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"xor">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );



parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'band'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"band">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'bor'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"bor">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'bxor'}, [L,R]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"bxor">>),false),
         [parseFunctionBody(noreturn,Params,L),parseFunctionBody(noreturn,Params,R)]
     );
parseFunctionBody(noreturn,Params,{c_call, _, {_, _, erlang}, {_, _, 'bnot'}, [T]})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(<<"erlang">>),estree:identifier(<<"bnot">>),false),
         [parseFunctionBody(noreturn,Params,T)]
     );



%Bodge job: external module calls
%parseFunctionBody(noreturn,Params,{c_call, _, {_, _, io}, {_, _, format}, [T]})->
%    esast:callExpression(
%         estree:memberExpression(estree:identifier(<<"console">>),estree:identifier(<<"log">>),false),
%         [parseFunctionBody(noreturn,Params,T)]
%     );


parseFunctionBody(noreturn,Params,{c_call, _, {_, _, Module}, {_, _, FunctionName}, Parameters})->
    estree:callExpression(
         estree:memberExpression(estree:identifier(atom_to_binary(Module,utf8)),estree:identifier(atom_to_binary(FunctionName,utf8)),false),
         lists:map(fun(T)->parseFunctionBody(noreturn,Parameters,T) end,Parameters)
     );


parseFunctionBody(return,Params,{c_values, _, _Values})->
    %io:format("~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]);

parseFunctionBody(return,Params,{c_var, A, Var})->
    estree:returnStatement(parseFunctionBody(noreturn,Params,{c_var, A, Var}));
parseFunctionBody(noreturn,Params,{c_var, _, Var})->
    estree:identifier(atom_to_binary(Var,utf8));
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
        estree:variableDeclaration([estree:variableDeclarator(estree:identifier(atom_to_binary(Variable,utf8)),parseFunctionBody(noreturn,Params,Value))],<<"let">>),
        parseFunctionBody(ReturnAtom,Params,UsedBy));

% Is apply a local function call? Assignment from function? Assignment with pattern matching?
parseFunctionBody(ReturnAtom,Params,{c_apply, _, {_,_,{FunctionName,Arity}}, Parameters})->
    parseFunctionBody(ReturnAtom,Params,{c_call, [], {a, a, exports}, {a, a, FunctionName}, Parameters});

parseFunctionBody(ReturnAtom,Params,{c_apply, _, {_, _, FunctionName}, Parameters})->
    parseFunctionBody(ReturnAtom,Params,{c_call, [], {a, a, exports}, {a, a, FunctionName}, Parameters});

parseFunctionBody(return,Params,{c_literal,_,Value})->
    estree:returnStatement(parseFunctionBody(noreturn,Params,{c_literal,[],Value}));
parseFunctionBody(noreturn,Params,{c_literal,_,Value}) when is_number(Value)->
    estree:newExpression(estree:identifier(<<"ErlNumber">>),[estree:literal(Value)]);
parseFunctionBody(noreturn,Params,{c_literal,_,Value}) when is_atom(Value)->
    estree:newExpression(estree:identifier(<<"Atom">>),[estree:literal(atom_to_binary(Value, utf8))]);
parseFunctionBody(noreturn,Params,{c_literal,_,Value}) when is_list(Value)->
    estree:newExpression(estree:identifier(<<"List">>),[estree:literal(Value)]);

parseFunctionBody(return,Params,A={c_tuple,_,Values})->
    estree:returnStatement(parseFunctionBody(noreturn,Params,A));
parseFunctionBody(noreturn,Params,{c_tuple,_,Values})->
    estree:newExpression(estree:identifier(<<"Tuple">>),[parseFunctionBody(noreturn,Params,Value) || Value <- Values]);




%List constructor
parseFunctionBody(return,Params,{c_cons,_,A,B})->
    estree:returnStatement(parseFunctionBody(noreturn,Params,{c_cons,[],A,B}));

parseFunctionBody(noreturn,Params,{c_cons,_,A,B={c_cons,_,C,D}})->
    estree:newExpression(estree:identifier(<<"List">>),
        parseConsChain(noreturn,Params,{c_cons,[],A,B})
   );

parseFunctionBody(noreturn,Params,{c_cons,_,A,B})->
    estree:newExpression(estree:identifier(<<"List">>),[parseFunctionBody(noreturn,Params,A),parseFunctionBody(noreturn,Params,B)]);



parseFunctionBody(return,Params,{c_primop,_,{_,_,Type},_Details})->
    % io:format("        Error? ~p~n~p~n", [Type,Details]),
    estree:error(atom_to_list(Type),"TODO Errors dont parse nicely\\n",estree:literal(<<"Message">>));
    % io:format("",[]);


parseFunctionBody(ReturnAtom,Params,{c_case, _, {c_var,_,Var}, Clauses})->
    parseFunctionBody(ReturnAtom,Params,{c_case, a, {c_values,a,[{c_var,a,Var}]}, Clauses});

parseFunctionBody(ReturnAtom,Params,{c_case, _, {c_values,_,Vars}, Clauses})->
    {UnboundVars,CaseClauses} = parseCaseClauses(ReturnAtom,Params, Vars, Clauses),
    case UnboundVars of
        [] -> CaseClauses;
        _  -> assembleSequence(estree:variableDeclaration(UnboundVars,<<"let">>),CaseClauses)
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
     estree:ifStatement(
        assembleCaseCondition(Params,Vars,Match,Evaluate),%test
        estree:blockStatement(%consequent
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
    estree:callExpression(
         estree:functionExpression(
              null,
              [],
              estree:blockStatement([parseFunctionBody(return,Params,Evaluate)]),
              false),
         []);
assembleCaseCondition(Params,Vars,Match,{c_literal,_,true})->
    assembleCaseCondition(Params,Vars,Match);
assembleCaseCondition(Params,Vars,Match,Evaluate)->
    Identifiers = lists:map(fun(Elem)->
            case Elem of
                {c_var,_,Name} -> parseFunctionBody(noreturn,Params,{c_var,[],Name});
                {c_alias,_,{c_var,_,Name},_Value} -> parseFunctionBody(noreturn,Params,{c_var,[],Name})
            end
        end,Match),
    estree:logicalExpression(
        <<"&&">>,
        assembleCaseCondition(Params,Vars,Match),
        estree:callExpression(
             estree:functionExpression(
                  null,
                  Identifiers,
                  estree:blockStatement([parseFunctionBody(return,Params,Evaluate)]),
                  false),
             Identifiers)
   ).

assembleCaseCondition(Params,[V],[M={c_var,_A,_N}])->
        parseFunctionBody(noreturn,Params,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [M,V]});
assembleCaseCondition(Params,[V],[_M={c_alias,_A,_N,Value}])->
        parseFunctionBody(noreturn,Params,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [Value,V]});
assembleCaseCondition(Params,[V|Vars],[M|Match])->
    estree:logicalExpression(<<"&&">>,
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
    [estree:expressionStatement(
        estree:assignmentExpression(
            <<"=">>,
            estree:identifier(atom_to_binary(Match,utf8)),
            estree:identifier(atom_to_binary(Variable,utf8))
        )
    )];
assignMatchedVars(Params,{c_var,_,Variable},{c_alias,_,{c_var,[],Name},_Value})->
    [estree:expressionStatement(
        estree:assignmentExpression(
            <<"=">>,
            estree:identifier(atom_to_binary(Name,utf8)),
            estree:identifier(atom_to_binary(Variable,utf8))
        )
    )];
assignMatchedVars(Params,A,B)->
    erlang:error(io_lib:format("assignMatchedVars error:~p~n~p",[A,B])),
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
            {c_var,_,Name} -> {true,estree:variableDeclarator(estree:identifier(atom_to_binary(Name,utf8)),estree:identifier(<<"undefined">>))};
            {c_alias,_,{c_var,_,Name},Value} -> {true,
                                                 estree:variableDeclarator(
                                                     estree:identifier(atom_to_binary(Name,utf8)),
                                                     parseFunctionBody(noreturn,[],Value))};
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
