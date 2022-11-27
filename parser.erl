-module(parser).

-compile(export_all).

evaluate_expression() ->
    % Reading and parsing the code
    {ok, File} = file:read_file("covered.erl"),
    Expression = unicode:characters_to_list(File),
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),

    % Create a counter where each position is a branch
    SizeP = length(Parsed),
    Counter = counters:new(SizeP, [atomics]),
    % Add the ccounter to the persistent_term
    persistent_term:put(1, Counter),

    % Instrumenting the code
    Parsedin = [],
    ParsedinF = parsed_in(Parsed, SizeP, 1, Parsedin),

    % Execute the code and see the result
    {value, Result, _} = erl_eval:exprs(ParsedinF, []),
    get_result(SizeP, Parsed, 1),
    Result.

% Instrument each branch individually, recursively
parsed_in(Parsed, SizeP, Now, Parsedin) ->
    if
        Now =< SizeP ->
            ParsedinF = Parsedin ++ [instrumenting(Parsed, Now)],
            parsed_in(Parsed, SizeP, Now+1, ParsedinF);
        true ->
            Parsedin
    end.


% Use the result of the instrumentantion and print the clauses that where executed in each branch
get_result(SizeP, Parsed, Now) ->
    if
        Now =< SizeP ->
            Branch = tuple_to_list(lists:nth(Now, Parsed)),
            Cond = lists:nth(1, Branch),
            Line = lists:nth(2, Branch),
            if 
                Cond == 'if' ->
                    % Print the branch name in this case the 'if' and its line
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(3, Branch),
                    % Print the branch clauses differentiating what was and what was not executed
                    print_clauses_if(ClauseList, counters:get(persistent_term:get(1),Now), length(ClauseList)+1, 1),
                    io:fwrite("~n~n"),
                    get_result(SizeP, Parsed, Now + 1);
                Cond == 'case' ->
                    % Print the branch name in this case the 'case' and its line
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(4, Branch),
                    % Print the branch clauses differentiating what was and what was not executed
                    print_clauses_case(ClauseList, counters:get(persistent_term:get(1),Now), length(ClauseList)+1, 1),
                    io:fwrite("~n~n"),
                    get_result(SizeP, Parsed, Now + 1);
                true ->
                    io:fwrite("Nada ~n")
            end;
        true ->
            ok
    end.

% ------------------------------------------ PRINT CLAUSES------------------------------------------------------------

% For each clause of the if it prints if it was or not executed
print_clauses_if(Clauses, Num, Length, Current) ->
    if
        Current == Num ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Opr = lists:nth(4, Expr),
            Line = lists:nth(2, Expr),
            io:format("~p/~p -> ", [Opr, Line]),
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
            print_clauses_case(Clauses, Num, Length, Current + 1);
        Current == Length ->
            ok;
        true ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Line = lists:nth(2, Expr),
            io:fwrite("Clause not executed/~p -> ", [Line]),
            print_clauses_case(Clauses, Num, Length, Current + 1)
    end.
% ------------------------------------------ PRINT CLAUSES/ ------------------------------------------------------------

% ------------------------------------- Automatic Instrumentation ------------------------------------------------------

% Returns the Branch with its clauses insturmented
instrumenting(Parsed, Now) ->
    Tipe = tuple_to_list(lists:nth(Now, Parsed)),
    Cond = lists:nth(1, Tipe),
    if 
        Cond == 'if' ->
            ClauseList = lists:nth(3, Tipe),
            Clauselistin = instrument_clauses(ClauseList, length(ClauseList)+1, 1, Now),
            Clauselistin2 = erlang:delete_element(length(Clauselistin), list_to_tuple(Clauselistin)),
            Clauselistin3 = tuple_to_list(Clauselistin2),
            Parsedin = setelement(3, lists:nth(Now, Parsed), Clauselistin3),
            Parsedin;
        Cond == 'case' ->
            ClauseList = lists:nth(4, Tipe),
            Clauselistin = instrument_clauses(ClauseList, length(ClauseList)+1, 1, Now),
            Clauselistin2 = erlang:delete_element(length(Clauselistin), list_to_tuple(Clauselistin)),
            Clauselistin3 = tuple_to_list(Clauselistin2),
            Parsedin = setelement(4, lists:nth(Now, Parsed), Clauselistin3),
            Parsedin;
        true ->
            io:fwrite("Nada ~n")
    end.

instrument_clauses(ClauseList, Length, Current, Now) ->
    if
        Current < Length ->
            Clause = tuple_to_list(lists:nth(Current, ClauseList)),
            Line = lists:nth(2, Clause),
            % adding to target code the AST of a code that adds the clause number to the counter
            Instrumentation = [{call,Line,{remote,Line,{atom,Line,counters},{atom,Line,add}},[{call,Line,{remote,Line,{atom,Line,persistent_term},{atom,Line,get}},[{integer,Line,1}]},{integer,Line,Now},{integer,Line,Current}]}],
            Item = Instrumentation ++ lists:nth(5, Clause),
            Clausein = setelement(5, lists:nth(Current, ClauseList), Item),
            Clauselistin = [Clausein] ++ instrument_clauses(ClauseList, Length, Current + 1, Now),
            Clauselistin;
        true ->
            Fim = [ok],
            [Fim]
    end.