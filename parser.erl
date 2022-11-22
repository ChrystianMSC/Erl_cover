-module(parser).

-compile(export_all).

parse_nested(Parsed) ->
    SizeP = length(Parsed),
    if
        SizeP == 1 ->
            Counter = counters:new(1, [atomics]),
            persistent_term:put(1, Counter),
            Parsedin = [instrumenting(Parsed)],
            {value, Result, _} = erl_eval:exprs(Parsedin, []),
            Tipe = tuple_to_list(lists:nth(1, Parsed)),
            Cond = lists:nth(1, Tipe),
            Line = lists:nth(2, Tipe),
            if 
                Cond == 'if' ->
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(3, Tipe),
                    % Getting the counter for printing if
                    print_clauses_if(ClauseList, counters:get(persistent_term:get(1),1), length(ClauseList)+1, 1),
                    io:fwrite("~n~n");
                Cond == 'case' ->
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(4, Tipe),
                    % Getting the counter for printing case (maybe not working)
                    print_clauses_case(ClauseList, counters:get(persistent_term:get(1),1), length(ClauseList)+1, 1),
                    io:fwrite("~n~n");
                true ->
                    io:fwrite("Nada ~n")
            end,
            io:format("           ~p~n~n", [Result]);
        true ->
            multi_branch(Parsed, SizeP, 1)
    end.


multi_branch(Parsed, SizeP, BranchN) ->
    if
        BranchN > SizeP ->
            ok;
        true ->
            Counter = counters:new(1, [atomics]),
            persistent_term:put(1, Counter),
            Parsedin = [instrumenting(Parsed)],
            {value, Result, _} = erl_eval:exprs(Parsedin, []),
            Tipe = tuple_to_list(lists:nth(BranchN, Parsed)),
            Cond = lists:nth(1, Tipe),
            Line = lists:nth(2, Tipe),
            if 
                Cond == 'if' ->
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(3, Tipe),
                    % Getting the counter for printing if
                    print_clauses_if(ClauseList, counters:get(persistent_term:get(1),1), length(ClauseList)+1, 1),
                    io:fwrite("~n~n");
                Cond == 'case' ->
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(4, Tipe),
                    % Getting the counter for printing case (maybe not working)
                    print_clauses_case(ClauseList, counters:get(persistent_term:get(1),1), length(ClauseList)+1, 1),
                    io:fwrite("~n~n");
                true ->
                    io:fwrite("Nada ~n")
            end,
            io:format("~p", [Result]),
            multi_branch(Parsed, SizeP, BranchN + 1)
    end.

evaluate_expression() ->
    % defining couter and adding it to persistent term
    {ok, File} = file:read_file("covered.erl"),
    Expression = unicode:characters_to_list(File),
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),

    SizeP = length(Parsed),

    if
        SizeP == 1 ->
            Counter = counters:new(1, [atomics]),
            persistent_term:put(1, Counter),
            Parsedin = [instrumenting(Parsed)],
            {value, Result, _} = erl_eval:exprs(Parsedin, []),
            Tipe = tuple_to_list(lists:nth(1, Parsed)),
            Cond = lists:nth(1, Tipe),
            Line = lists:nth(2, Tipe),
            if 
                Cond == 'if' ->
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(3, Tipe),
                    % Getting the counter for printing if
                    print_clauses_if(ClauseList, counters:get(persistent_term:get(1),1), length(ClauseList)+1, 1),
                    io:fwrite("~n~n");
                Cond == 'case' ->
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(4, Tipe),
                    % Getting the counter for printing case (maybe not working)
                    print_clauses_case(ClauseList, counters:get(persistent_term:get(1),1), length(ClauseList)+1, 1),
                    io:fwrite("~n~n");
                true ->
                    io:fwrite("Nada ~n")
            end,
            Result;
        true ->
            multi_branch(Parsed, SizeP, 1)
    end.

print_clauses_if(Clauses, Num, Length, Current) ->
    if
        Current == Num ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Opr = lists:nth(4, Expr),
            Line = lists:nth(2, Expr),
            io:format("~p/~p -> ", [Opr, Line]),
            Inside = lists:nth(5, Expr),
            Tipe = tuple_to_list(lists:nth(1, Inside)),
            Cond = lists:nth(1, Tipe),
            if
                Cond == 'if' ->
                    io:format("~n~n          "), 
                    parse_nested(Inside);
                Cond == 'case' ->
                    io:format("~n~n          "), 
                    parse_nested(Inside);
                true ->
                    ok
            end,
            print_clauses_if(Clauses, Num, Length, Current + 1);
        Current == Length ->
            ok;
        true ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Line = lists:nth(2, Expr),
            io:fwrite("Clause not executed/~p -> ", [Line]),
            print_clauses_if(Clauses, Num, Length, Current + 1)
    end.

print_clauses_case(Clauses, Num, Length, Current) ->
    if
        Current == Num ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Opr = lists:nth(3, Expr),
            Line = lists:nth(2, Expr),
            io:format("~p/~p -> ", [Opr, Line]),
            Inside = lists:nth(5, Expr),
            Tipe = tuple_to_list(lists:nth(1, Inside)),
            Cond = lists:nth(1, Tipe),
            if
                Cond == 'if' ->
                    io:format("~n~n          "), 
                    parse_nested(Inside);
                Cond == 'case' ->
                    io:format("~n~n          "), 
                    parse_nested(Inside);
                true ->
                    ok
            end,
            print_clauses_case(Clauses, Num, Length, Current + 1);
        Current == Length ->
            ok;
        true ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Line = lists:nth(2, Expr),
            io:fwrite("Clause not executed/~p -> ", [Line]),
            print_clauses_case(Clauses, Num, Length, Current + 1)
    end.

% ----------------------- Automatic Instrumentation ------------------------------ %

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
        Cond == 'case' ->
            ClauseList = lists:nth(4, Tipe),
            Clauselistin = instrument_clauses(ClauseList, length(ClauseList)+1, 1),
            Clauselistin2 = erlang:delete_element(length(Clauselistin), list_to_tuple(Clauselistin)),
            Clauselistin3 = tuple_to_list(Clauselistin2),
            Parsedin = setelement(4, lists:nth(1, Parsed), Clauselistin3),
            Parsedin;
        true ->
            io:fwrite("Nada ~n")
    end.

instrument_clauses(ClauseList, Length, Current) ->
    if
        Current < Length ->
            Clause = tuple_to_list(lists:nth(Current, ClauseList)),
            Line = lists:nth(2, Clause),
            % adding to target code the AST of a code that adds the clause number to the counter
            Instrumentation = [{call,Line,{remote,Line,{atom,Line,counters},{atom,Line,add}},[{call,Line,{remote,Line,{atom,Line,persistent_term},{atom,Line,get}},[{integer,Line,1}]},{integer,Line,1},{integer,Line,Current}]}],
            Item = Instrumentation ++ lists:nth(5, Clause),
            Clausein = setelement(5, lists:nth(Current, ClauseList), Item),
            Clauselistin = [Clausein] ++ instrument_clauses(ClauseList, Length, Current + 1),
            Clauselistin;
        true ->
            Fim = [ok],
            [Fim]
    end.