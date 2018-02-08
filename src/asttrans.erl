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


%###########################
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


%###########################
%Read a function
parseFunction({{_, _, {FunctionName, Arity}}, {_c_fun, [compiler_generated], _, _}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),estree:function_expression(null,[],estree:block_statement([estree:empty_statement()]),false)};
parseFunction({{_, _, {FunctionName, Arity}}, {_c_fun, _, ParamNames, Body}})->
    {atom_to_list(FunctionName)++"/"++integer_to_list(Arity),estree:function_expression(
        null,
        tupleListToIdentifierList(ParamNames,tupleList_getVars_3(ParamNames)),
        estree:block_statement(
            encapsulateExpressions(
                listCheck(
                    parseNode(return,tupleList_getVars_3(ParamNames),Body)
                )
            )
        ),
        false
    )}.


%###########################
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
                true->estree:expression_statement(X)
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


%###########################
parseNode(ReturnAtom,Params,N)->
    case tuple_getVar_1(N) of
        c_call ->parseCall(ReturnAtom,Params,N);
        c_values ->parseValues(ReturnAtom,Params,N);
        c_var ->parseVar(ReturnAtom,Params,N);
        c_seq ->parseSeq(ReturnAtom,Params,N);
        c_let ->parseLet(ReturnAtom,Params,N);
        c_apply ->parseApply(ReturnAtom,Params,N);
        c_literal ->parseLiteral(ReturnAtom,Params,N);
        c_tuple ->parseTuple(ReturnAtom,Params,N);
        c_cons ->parseCons(ReturnAtom,Params,N);
        c_try ->parseTry(ReturnAtom,Params,N);
        c_primop ->parsePrimop(ReturnAtom,Params,N);
        c_letrec ->parseLetrec(ReturnAtom,Params,N);
        c_case ->parseCase(ReturnAtom,Params,N)
    end.

%###########################
parseCall(return,Params,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters})->
    estree:return_statement(parseCall(noreturn,Params,{c_call, A, {B, C, Module}, {D, E, FunctionName}, Parameters}));

parseCall(noreturn,Params,{c_call, _, {_, _, Module}, {_, _, FunctionName}, Parameters})->
    estree:call_expression(
         estree:member_expression(estree:identifier(atom_to_binary(Module,utf8)),estree:literal(atom_to_binary(FunctionName,utf8)),true),
         lists:map(fun(T)->parseNode(noreturn,Parameters,T) end,Parameters)
     ).


%###########################
parseValues(return,Params,{c_values, _, _Values})->
    %io:format("~p~n", [tupleList_getVars_3(Values)]);
    io:format("",[]).


%###########################
parseVar(return,Params,{c_var, A, Var})->
    estree:return_statement(parseVar(noreturn,Params,{c_var, A, Var}));
%added for support of functions by name
parseVar(noreturn,Params,{c_var, _, {Name,Arity}})->
    estree:identifier(atom_to_list(Name)++"/"++integer_to_list(Arity));
parseVar(noreturn,Params,{c_var, _, Var})->
    estree:identifier(atom_to_binary(Var,utf8)).
    %io:format("",[]);


%###########################
parseSeq(return,Params,{c_seq, _, A, B})->
    assembleSequence(
        parseNode(noreturn,Params,A),
        parseNode(return,Params,B));
parseSeq(noreturn,Params,{c_seq, _, A, B})->
    assembleSequence(
        parseNode(noreturn,Params,A),
        parseNode(noreturn,Params,B)).


%###########################
% A let statement is the core representation of implicit variable declarations (the result of some function as an argument of another function
parseLet(ReturnAtom,Params,{c_let, _, [{_, _, Variable}], Value, UsedBy})->
    assembleSequence(
        estree:variable_declaration([estree:variable_declarator(estree:identifier(atom_to_binary(Variable,utf8)),parseNode(noreturn,Params,Value))],<<"let">>),
        parseNode(ReturnAtom,Params,UsedBy)).


%###########################
% Is apply a local function call? Assignment from function? Assignment with pattern matching?
parseApply(ReturnAtom,Params,{c_apply, _, {_,_,{FunctionName,Arity}}, Parameters})->
    % parseNode(ReturnAtom,Params,{c_call, [], {a, a, functions}, {a, a, FunctionName}, Parameters});
    parseCall(ReturnAtom,Params,{c_call, [], {a, a, functions}, {a, a, list_to_atom(atom_to_list(FunctionName)++"/"++integer_to_list(Arity))}, Parameters});
    % estree:call_expression(
    %      % estree:identifier(list_to_binary(atom_to_list(FunctionName)++"/"++integer_to_list(Arity))),
    %      estree:member_expression(
    %         estree:identifier(atom_to_binary(functions,utf8)),
    %         estree:literal(identify_normalise(atom_to_list(FunctionName)++"/"++integer_to_list(Arity))),
    %         true),
    %      lists:map(fun(T)->parseNode(noreturn,Parameters,T) end,Parameters)
    %  );

parseApply(ReturnAtom,Params,{c_apply, _, {_, _, FunctionName}, Parameters})->
    parseCall(ReturnAtom,Params,{c_call, [], {a, a, functions}, {a, a, FunctionName}, Parameters}).


%###########################
parseLiteral(return,Params,{c_literal,_,Value})->
    estree:return_statement(parseLiteral(noreturn,Params,{c_literal,[],Value}));
parseLiteral(noreturn,Params,{c_literal,_,Value}) when is_number(Value)->
    estree:new_expression(estree:identifier(<<"ErlNumber">>),[estree:literal(Value)]);
parseLiteral(noreturn,Params,{c_literal,_,Value}) when is_atom(Value)->
    estree:new_expression(estree:identifier(<<"Atom">>),[estree:literal(atom_to_binary(Value, utf8))]);
parseLiteral(noreturn,Params,{c_literal,_,Value}) when is_list(Value)->
    estree:new_expression(estree:identifier(<<"List">>),[estree:literal(Value)]).


%###########################
parseTuple(return,Params,A={c_tuple,_,Values})->
    estree:return_statement(parseTuple(noreturn,Params,A));
parseTuple(noreturn,Params,{c_tuple,_,Values})->
    estree:new_expression(estree:identifier(<<"Tuple">>),[parseNode(noreturn,Params,Value) || Value <- Values]).


%###########################
%List constructor
parseCons(return,Params,{c_cons,_,A,B})->
    estree:return_statement(parseCons(noreturn,Params,{c_cons,[],A,B}));

parseCons(noreturn,Params,{c_cons,_,A,B={c_cons,_,C,D}})->
    estree:new_expression(estree:identifier(<<"List">>),
        parseConsChain(noreturn,Params,{c_cons,[],A,B})
   );

parseCons(noreturn,Params,{c_cons,_,A,B})->
    estree:new_expression(estree:identifier(<<"List">>),[parseNode(noreturn,Params,A),parseNode(noreturn,Params,B)]).


%###########################
%For lack of a more apparent reason for the c_try token I'm treating it a superfluous encapsulation
parseTry(return,Params,{c_try,_,Elem,_,_,_,_})->
    estree:return_statement(parseTry(noreturn,Params,{c_try,[],Elem,a,a,a,a}));
parseTry(noreturn,Params,{c_try,_,Elem,_,_,_,_})->
    estree:call_expression(
        estree:function_expression(
            null,
            [],
            estree:block_statement(
                encapsulateExpressions(
                    listCheck(
                        parseNode(return,Params,Elem)
                    )
                )
            ),
        false),
    []).
    % parseNode(ReturnAtom,Params,Elem);


%###########################
parsePrimop(return,Params,{c_primop,_,{_,_,Type},_Details})->
    % io:format("        Error? ~p~n~p~n", [Type,Details]),
    estree:error(atom_to_list(Type),"TODO Errors dont parse nicely\\n",estree:literal(<<"Message">>)).
    % io:format("",[]);


%###########################
%c_letrec appears to represent list comprehension.
parseLetrec(ReturnAtom,Params,{c_letrec,_,[Func],Apply})->
% parseNode(ReturnAtom,Params,{c_letrec,_,[{{_, _, {FunctionName, Arity}}, {_c_fun, _, ParamNames, Body}}],Apply})->
%     {Id,F} = parseFunction({{a, [], {'listComp', Arity}}, {c_fun, [], ParamNames, Body}}),
    {Id,F} = parseFunction(Func),
    assembleSequence(
        %estree:variable_declaration(estree:variable_declarator(estree:identifier(list_to_binary(Id)),F),<<"let">>),
        % estree:let_declaration(list_to_binary(Id),F),
        estree:assignment_expression(
            <<"=">>,
            estree:member_expression(
                estree:identifier(<<"functions">>),
                estree:literal(identify_normalise(Id)),
                % estree:identifier(<<"listComp">>),
                true
            ),
            F
        ),
        % estree:function_expression(
        %     estree:identifier(identify_normalise(atom_to_list(FunctionName)++"/"++integer_to_list(Arity))),
        %     tupleListToIdentifierList(ParamNames,tupleList_getVars_3(ParamNames)),
        %     estree:block_statement(
        %         encapsulateExpressions(
        %             listCheck(
        %                 parseNode(return,tupleList_getVars_3(ParamNames),Body)
        %             )
        %         )
        %     ),
        %     true
        % ),
        parseNode(ReturnAtom,Params,Apply)
    ).


%###########################
parseCase(ReturnAtom,Params,{c_case, _, {c_var,_,Var}, Clauses})->
    parseCase(ReturnAtom,Params,{c_case, a, {c_values,a,[{c_var,a,Var}]}, Clauses});

parseCase(ReturnAtom,Params,{c_case, _, {c_values,_,Vars}, Clauses})->
    {UnboundVars,CaseClauses} = parseCaseClauses(ReturnAtom,Params, Vars, Clauses),
    case UnboundVars of
        [] -> CaseClauses;
        _  -> assembleSequence(
                estree:variable_declaration(
                    tupleList_getVars_2(maps:to_list(maps:from_list(UnboundVars))),
                    <<"let">>),
                CaseClauses)
    end;

parseCase(ReturnAtom,Params,{c_case, _, {c_apply,_,{c_var,_,Fun},Args}, Clauses})->
    case Fun of
        {Name,_} -> Fun_Actual = Name;
        _ -> Fun_Actual = Fun
    end,
    parseFunctionCase(ReturnAtom,Params,
        estree:call_expression(
            estree:identifier(atom_to_binary(Fun_Actual,utf8)),
            lists:map(fun(T)->parseNode(noreturn,Params,T) end,Args)
        ),
        Clauses
    );

parseCase(ReturnAtom,Params,{c_case, _, {c_call,_,{c_literal,_,Module},{c_literal,_,FunctionName},Args}, Clauses})->
    parseFunctionCase(ReturnAtom,Params,
        estree:call_expression(
            estree:member_expression(estree:identifier(atom_to_binary(Module,utf8)),estree:identifier(atom_to_binary(FunctionName,utf8)),false),
            lists:map(fun(T)->parseNode(noreturn,Params,T) end,Args)
        ),
        Clauses
    ).


parseFunctionCase(ReturnAtom,Params,FuncCall, Clauses)->
    assembleSequence(
        %Define temp variable & call function
        estree:variable_declaration([estree:variable_declarator(
            estree:identifier(atom_to_binary('_tempVar',utf8)),
            FuncCall
        )],<<"let">>),
        %Continue as normal, passing the temp variable
        parseCase(ReturnAtom,Params,{c_case, [], {c_var,[],'_tempVar'}, Clauses})
    ).



parseConsChain(noreturn,Params,{c_cons,[],A,{c_cons,_,C,D}})->
    [parseNode(noreturn,Params,A)|parseConsChain(noreturn,Params,{c_cons,[],C,D})];
parseConsChain(noreturn,Params,{c_cons,[],A,B})->
    [parseNode(noreturn,Params,A),parseNode(noreturn,Params,B)].





parseCaseClauses(ReturnAtom,Params, Vars, [])->
    {[],[]};
parseCaseClauses(ReturnAtom,Params, Vars, [{c_clause,_,Match,Evaluate,Consequent}|Clauses])->
    {UnboundVars,ElseClauses} = parseCaseClauses(ReturnAtom,Params, Vars, Clauses),%alternate
    case ElseClauses of
        [] -> ElseClausesActual = null;
        _  -> ElseClausesActual = ElseClauses
    end,
    {lists:append(declaratorsFromList(Match),UnboundVars),
     estree:if_statement(
        assembleCaseCondition(Params,Vars,Match,Evaluate),%test
        estree:block_statement(%consequent
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
                        parseNode(ReturnAtom,Params,Consequent)
                    )
                )
            )
        ),
        ElseClausesActual %alternate
    )}.

assembleCaseCondition(Params,_,[],Evaluate)->
    estree:call_expression(
         estree:function_expression(
              null,
              [],
              estree:block_statement(
                encapsulateExpressions(
                    listCheck(
                        parseNode(return,Params,Evaluate)
                    )
                )
              ),
              false),
         []);
assembleCaseCondition(Params,Vars,Match,{c_literal,_,true})->
    assembleCaseCondition(Params,Vars,Match);
assembleCaseCondition(Params,Vars,Match,Evaluate)->
%    Identifiers = tupleListToIdentifierList(Match,Params),
    Identifiers = lists:map(fun(Elem)->
            case Elem of
                {c_alias,_,{c_var,_,Name},_Value} -> parseVar(noreturn,Params,{c_var,[],Name});
                _ -> parseNode(noreturn,Params,Elem)
            end
        end,Match),
    estree:logical_expression(
        <<"&&">>,
        assembleCaseCondition(Params,Vars,Match),
        estree:call_expression(
             estree:function_expression(
                  null,
                  Identifiers,
                    estree:block_statement(
                      encapsulateExpressions(
                          listCheck(
                              parseNode(return,Params,Evaluate)
                          )
                      )
                    ),
                  false),
             Identifiers)
   ).
assembleCaseCondition(Params,[V],[_M={c_alias,_A,_N,Value}])->
        parseCall(noreturn,Params,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [Value,V]});
assembleCaseCondition(Params,[V],[M])->
%assembleCaseCondition(Params,[V],[M={c_var,A,N}])->
        parseCall(noreturn,Params,{c_call, a, {a, a, erlang}, {a, a, 'match'}, [M,V]});
assembleCaseCondition(Params,[V|Vars],[M|Match])->
    estree:logical_expression(<<"&&">>,
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
    [estree:expression_statement(
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Match,utf8)),
            estree:identifier(atom_to_binary(Variable,utf8))
        )
    )];
assignMatchedVars(Params,{c_var,_,Variable},{c_alias,_,{c_var,[],Name},_Value})->
    [estree:expression_statement(
        estree:assignment_expression(
            <<"=">>,
            estree:identifier(atom_to_binary(Name,utf8)),
            estree:identifier(atom_to_binary(Variable,utf8))
        )
    )];
%assignMatchedVars(Params,A,B)->
%    erlang:error(io_lib:format("assignMatchedVars error:~p~n~p",[A,B])),
assignMatchedVars(Params,_,_)->
    [ok].







%###########################
assembleSequence(L,R) when is_list(L) and is_list(R)->
    lists:append(L,R);
assembleSequence(L,R) when is_list(R)->
    [L|R];
assembleSequence(L,R) when is_list(L)->
    lists:append(L,[R]);
assembleSequence(L,R)->
    [L,R].


tupleListToIdentifierList(List,Params)->
    lists:map(fun({c_var,[],A})->parseVar(noreturn,Params,{c_var,[],A}) end,List).



tupleList_getVars_3([])->
    [];
tupleList_getVars_3([{_,_, Val} | Body])->
    [Val | tupleList_getVars_3(Body)];
tupleList_getVars_3([{_, _, Val, _} | Body])->
    [Val | tupleList_getVars_3(Body)].


tupleList_getVars_2([])->
    [];
tupleList_getVars_2([{_, Val} | Body])->
    [Val | tupleList_getVars_2(Body)];
tupleList_getVars_2([{_, Val, _} | Body])->
    [Val | tupleList_getVars_2(Body)].


tuple_getVar_1({V})->V;
tuple_getVar_1({V,_})->V;
tuple_getVar_1({V,_,_})->V;
tuple_getVar_1({V,_,_,_})->V;
tuple_getVar_1({V,_,_,_,_})->V;
tuple_getVar_1({V,_,_,_,_,_})->V;
tuple_getVar_1({V,_,_,_,_,_,_})->V.


declaratorsFromList(List)->
    lists:filtermap(fun(Elem)->
        case Elem of
            {c_var,_,Name} -> { true,{Name,
                                estree:variable_declarator(
                                    estree:identifier(atom_to_binary(Name,utf8)),
                                    estree:identifier(<<"undefined">>))}};
            {c_alias,_,{c_var,_,Name},Value} -> {true,{Name,
                                                 estree:variable_declarator(
                                                     estree:identifier(atom_to_binary(Name,utf8)),
                                                     parseNode(noreturn,[],Value))}};
            _              -> false
        end
    end,List).


identify_normalise(N)->
    % {ok, Regex}=re:compile("[^A-Za-z0-9_$]"),
    % iolist_to_binary(re:replace(N,Regex,"_$_",[global])).
    list_to_binary(N).

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
