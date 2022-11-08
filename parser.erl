-module(parser).

-compile(export_all).

evaluate_expression() ->
    {ok, File} = file:read_file("covered.erl"),
    Expression = unicode:characters_to_list(File),
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    Parsedin = [instrumenting(Parsed)],
    {value, Result, _} = erl_eval:exprs(Parsedin, []),
    receive
        {clause, Num} -> io:format("Chamei clausula ~p ~n~n", [Num])
    end,
    Tipe = tuple_to_list(lists:nth(1, Parsed)),
    Cond = lists:nth(1, Tipe),
    Line = lists:nth(2, Tipe),
    if 
        Cond == 'if' ->
            io:format("~p/~p -> ", [Cond, Line]),
            ClauseList = lists:nth(3, Tipe),
            print_clauses(ClauseList, Num, length(ClauseList)+1, 1),
            io:fwrite("~n~n");
        true ->
            io:fwrite("Nada ~n")
    end,
    Parsedin.

print_clauses(Clauses, Num, Length, Current) ->
    if
        Current == Num ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Opr = lists:nth(4, Expr),
            Line = lists:nth(2, Expr),
            io:format("~p****/~p -> ", [Opr, Line]),
            print_clauses(Clauses, Num, Length, Current + 1);
        Current == Length ->
            ok;
        true ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Line = lists:nth(2, Expr),
            io:fwrite("Clause not executed/~p -> ", [Line]),
            print_clauses(Clauses, Num, Length, Current + 1)
    end.

% ---------------------------------------------------------- Automatic Instrumamtation ----------------------------------------------------------------- %

instrumenting(Parsed) ->
    Tipe = tuple_to_list(lists:nth(1, Parsed)),
    Cond = lists:nth(1, Tipe),
    if 
        Cond == 'if' ->
            ClauseList = lists:nth(3, Tipe),
            Clauselistin = instrument_clauses(ClauseList, length(ClauseList)+1, 1),
            Clauselistin2 = erlang:delete_element(length(Clauselistin), list_to_tuple(Clauselistin)),
            Clauselistin3 = tuple_to_list(Clauselistin2),
            Parsedin = setelement(3, lists:nth(1, Parsed), Clauselistin3),
            Parsedin;
        true ->
            io:fwrite("Nada ~n")
    end.

instrument_clauses(ClauseList, Length, Current) ->
    if
        Current < Length ->
            Clause = tuple_to_list(lists:nth(Current, ClauseList)),
            Line = lists:nth(2, Clause),
            Instrumentation = [{op,Line,'!',{call,Line,{atom,Line,self},[]},{tuple,Line,[{atom,Line,clause},{integer,Line,Current}]}}],
            Item = Instrumentation ++ lists:nth(5, Clause),
            Clausein = setelement(5, lists:nth(Current, ClauseList), Item),
            Clauselistin = [Clausein] ++ instrument_clauses(ClauseList, Length, Current + 1),
            Clauselistin;
        true ->
            Fim = [ok],
            [Fim]
    end.