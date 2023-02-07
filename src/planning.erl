-module(planning).

-compile(export_all).

parser(FileName) ->
    {ok, File} = file:read_file(FileName),
    Expression = unicode:characters_to_list(File),
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    Parsed.

retorna_tupla(Elem) ->
    if
        is_list(Elem) ->
            [percorre_tupla(H) || H <- Elem];
        true ->
            Elem
    end.

imprimeResultado(ClausulaAtual, ExpressaoAtual, NumExpressoes) ->
    if
        ExpressaoAtual < NumExpressoes ->
            NovaCAtual = imprimeExpressao(ClausulaAtual, persistent_term:get(ExpressaoAtual), ExpressaoAtual, 1),
            imprimeResultado(NovaCAtual, ExpressaoAtual + 1, NumExpressoes);
        true ->
            ok
    end.

imprimeExpressao(ClausulaAtual, Lista, ExpressaoAtual, Count) ->
    Size = lists:nth(3, persistent_term:get(ExpressaoAtual)),
    if
        Count =< Size ->
            ListaAdd = Lista ++ [counters:get(persistent_term:get(contaExec), ClausulaAtual)],
            imprimeExpressao(ClausulaAtual + 1, ListaAdd, ExpressaoAtual, Count + 1);
        true ->
            io:format("~n~p", [Lista]),
            ClausulaAtual
    end.

instrumenta(Clausula) ->
    counters:add(persistent_term:get(contaClausula), 1, 1),

    Elementos = tuple_to_list(Clausula),
    Line = lists:nth(2, Elementos),

    ClausulaAtual = counters:get(persistent_term:get(contaClausula), 2),
    counters:add(persistent_term:get(contaClausula), 2, 1),

    Executaveis = lists:nth(5, Elementos),

    ExecutaviesInstr = [{call,Line,
                        {remote,Line,{atom,Line,counters},{atom,Line,add}},
                        [{call,Line,
                            {remote,Line,{atom,Line,persistent_term},{atom,Line,get}},
                            [{atom,Line,contaExec}]},
                        {integer,Line,ClausulaAtual},
                        {integer,Line,1}]}] ++ Executaveis,

    list_to_tuple(lists:sublist(Elementos,4) ++ [ExecutaviesInstr]).

percorre_clausula(Clausula) ->
    ClausulaLis = tuple_to_list(Clausula),
    Executaveis = lists:nth(5, ClausulaLis),
    ExecutaveisInstr = [percorre_tupla(H) || H <- Executaveis],
    list_to_tuple(lists:sublist(ClausulaLis,4) ++ [ExecutaveisInstr]).

trata_clausula(Num1, Num2, ListaTuplas) ->
    ClauseList = lists:nth(Num1, ListaTuplas),
    TuplaItr = lists:sublist(ListaTuplas, Num2) ++ [[instrumenta(H) || H <- ClauseList]],
    Clausulas = lists:nth(Num1, TuplaItr),ClausulaInstr = [percorre_clausula(H) || H <- Clausulas],
    TuplaInstr = lists:sublist(TuplaItr, Num2) ++ [ClausulaInstr],
    TuplaIteradainst = [percorre_tupla(H) || H <- TuplaInstr],
    list_to_tuple(TuplaIteradainst).

trata_Circuito(ListaTuplas) ->
    counters:add(persistent_term:get(contaClausula), 1, 1),

    Line = lists:nth(2, ListaTuplas),

    ClausulaAtual = counters:get(persistent_term:get(contaClausula), 2),
    counters:add(persistent_term:get(contaClausula), 2, 1),

    Direita = lists:nth(5, ListaTuplas),
    Esquerda = lists:sublist(ListaTuplas, 4),
    list_to_tuple(Esquerda ++ [{block,17,[{call,Line,
                        {remote,Line,{atom,Line,counters},{atom,Line,add}},
                        [{call,Line,
                            {remote,Line,{atom,Line,persistent_term},{atom,Line,get}},
                            [{atom,Line,contaExec}]},
                        {integer,Line,ClausulaAtual},
                        {integer,Line,1}]},Direita]}]).

percorre_tupla(Tupla) ->
    if 
        is_tuple(Tupla) ->
            ListaTuplas = tuple_to_list(Tupla),
            NodeName = lists:nth(1, ListaTuplas),
            AndOr = lists:nth(3, ListaTuplas),
            if 
                NodeName == 'if' -> 

                    Guarda = ['if', lists:nth(2, ListaTuplas), length(lists:nth(3, ListaTuplas))],
                    persistent_term:put(counters:get(persistent_term:get(contaEspressao), 1), Guarda),
                    counters:add(persistent_term:get(contaEspressao), 1, 1),

                    trata_clausula(3, 2, ListaTuplas);
                NodeName == 'case' -> 

                    Guarda = ['case', lists:nth(2, ListaTuplas), length(lists:nth(4, ListaTuplas))],
                    persistent_term:put(counters:get(persistent_term:get(contaEspressao), 1), Guarda),
                    counters:add(persistent_term:get(contaEspressao), 1, 1),

                    trata_clausula(4, 3, ListaTuplas);
                AndOr == 'andalso' orelse AndOr == 'orelse' -> 
                    
                    Guarda = [AndOr, lists:nth(2, ListaTuplas), 1],
                    persistent_term:put(counters:get(persistent_term:get(contaEspressao), 1), Guarda),
                    counters:add(persistent_term:get(contaEspressao), 1, 1),

                    trata_Circuito(ListaTuplas);
                true ->
                    TuplaIterada = [retorna_tupla(H) || H <- ListaTuplas],
                    list_to_tuple(TuplaIterada)
            end;
        true ->
            Tupla
    end.


percorre_ast(FileName) ->

    Contador = counters:new(2, [atomics]),
    persistent_term:put(contaClausula, Contador),
    counters:add(persistent_term:get(contaClausula), 2, 1),

    ContaEspressao = counters:new(1, [atomics]),
    persistent_term:put(contaEspressao, ContaEspressao),
    counters:add(persistent_term:get(contaEspressao), 1, 1),

    Parsed = parser(FileName),

    ParsedIterado = [percorre_tupla(H) || H <- Parsed],

    % io:format("~p", [Parsed]),

    ContaExec = counters:new(counters:get(persistent_term:get(contaClausula), 1), [atomics]),
    persistent_term:put(contaExec, ContaExec),
    
    io:format("~nNumero de clausulas: ~p~n", [counters:get(persistent_term:get(contaClausula), 1)]),
    

    {value, Result, _} = erl_eval:exprs(ParsedIterado, []),

    imprimeResultado(1, 1,counters:get(persistent_term:get(contaEspressao), 1)).


