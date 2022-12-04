-module(parser).

-compile(export_all).

branch_filter(Parsed, SizeP, CurrentBranch) ->
    if
        CurrentBranch =< SizeP ->
            Tipe = tuple_to_list(lists:nth(CurrentBranch, Parsed)),
            Cond = lists:nth(1, Tipe),
            if 
                Cond == 'if' ->
                    branch_filter(Parsed, SizeP, CurrentBranch + 1);
                Cond == 'case' ->
                    branch_filter(Parsed, SizeP, CurrentBranch + 1);
                true ->
                    FilteredParsed = erlang:delete_element(CurrentBranch, list_to_tuple(Parsed)),
                    FilteredParsed2 = tuple_to_list(FilteredParsed),
                    if
                        CurrentBranch == 1 ->
                            branch_filter(FilteredParsed2, SizeP - 1, CurrentBranch);
                        true ->
                            branch_filter(FilteredParsed2, SizeP - 1, CurrentBranch - 1)
                    end
            end;
        true ->
            Parsed
    end.




parser(FileName) ->
    {ok, File} = file:read_file(FileName),
    Expression = unicode:characters_to_list(File),
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    Parsed.


evaluate_expression(FileName) ->
    % Reading and parsing the code
    Parsed = parser(FileName),

    SizeP = length(Parsed),

    FilteredParsed = branch_filter(Parsed, SizeP, 1),

    SizeFiltered = length(FilteredParsed),

    InstrumentedP = instrument(FilteredParsed, SizeFiltered),

    % Execute the code and see the result
    {value, Result, _} = erl_eval:exprs(InstrumentedP, []),
    get_result(SizeFiltered, FilteredParsed, 1),
    Result.

instrument(Parsed, SizeP) ->
    % Create a counter where each position is a branch
    Counter = counters:new(SizeP, [atomics]),
    % Add the ccounter to the persistent_term
    persistent_term:put(1, Counter),
    % Instrumenting the code
    Parsedin = [],



    % TODO, filtar o que é branch
    
        

    ParsedinF = parsed_in(Parsed, SizeP, 1, Parsedin),
    ParsedinF.


% Instrument each branch individually, recursively
parsed_in(Parsed, SizeP, CurrentBranch, Parsedin) ->
    if
        CurrentBranch =< SizeP ->
            ParsedinF = Parsedin ++ [instrumenting(Parsed, CurrentBranch)],
            parsed_in(Parsed, SizeP, CurrentBranch+1, ParsedinF);
        true ->
            Parsedin
    end.


% Use the result of the instrumentantion and print the clauses that where executed in each branch
get_result(SizeP, Parsed, CurrentBranch) ->
    if
        CurrentBranch =< SizeP ->
            Branch = tuple_to_list(lists:nth(CurrentBranch, Parsed)),
            Cond = lists:nth(1, Branch),
            Line = lists:nth(2, Branch),
            if 
                Cond == 'if' ->
                    % Print the branch name in this case the 'if' and its line
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(3, Branch),
                    % Print the branch clauses differentiating what was and what was not executed
                    print_clauses_if(ClauseList, counters:get(persistent_term:get(1),CurrentBranch), length(ClauseList)+1, 1),
                    io:fwrite("~n~n"),
                    get_result(SizeP, Parsed, CurrentBranch + 1);
                Cond == 'case' ->
                    % Print the branch name in this case the 'case' and its line
                    io:format("~p/~p -> ", [Cond, Line]),
                    ClauseList = lists:nth(4, Branch),
                    % Print the branch clauses differentiating what was and what was not executed
                    print_clauses_case(ClauseList, counters:get(persistent_term:get(1),CurrentBranch), length(ClauseList)+1, 1),
                    io:fwrite("~n~n"),
                    get_result(SizeP, Parsed, CurrentBranch + 1);
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
instrumenting(Parsed, CurrentBranch) ->
    BranchList = list_to_tuple(Parsed),
    Branch = element(CurrentBranch, BranchList),
    Cond = element(1, Branch),
    if 
        Cond == 'if' ->
            ClauseList = element(3, Branch),
            Clauselistin = instrument_clauses(ClauseList, length(ClauseList)+1, 1, CurrentBranch),
            Clauselistin2 = erlang:delete_element(length(Clauselistin), list_to_tuple(Clauselistin)),
            Clauselistin3 = tuple_to_list(Clauselistin2),
            Parsedin = setelement(3, lists:nth(CurrentBranch, Parsed), Clauselistin3),
            Parsedin;
        Cond == 'case' ->
            ClauseList = element(4, Branch),
            Clauselistin = instrument_clauses(ClauseList, length(ClauseList)+1, 1, CurrentBranch),
            Clauselistin2 = erlang:delete_element(length(Clauselistin), list_to_tuple(Clauselistin)),
            Clauselistin3 = tuple_to_list(Clauselistin2),
            Parsedin = setelement(4, lists:nth(CurrentBranch, Parsed), Clauselistin3),
            Parsedin;
        true ->
            io:fwrite("Nada ~n")
    end.

instrument_clauses(ClauseList, Length, Current, CurrentBranch) ->
    if
        Current < Length ->
            Clause = tuple_to_list(lists:nth(Current, ClauseList)),
            Line = lists:nth(2, Clause),
            % adding to target code the AST of a code that adds the clause number to the counter
            Instrumentation = [{call,Line,{remote,Line,{atom,Line,counters},{atom,Line,add}},[{call,Line,{remote,Line,{atom,Line,persistent_term},{atom,Line,get}},[{integer,Line,1}]},{integer,Line,CurrentBranch},{integer,Line,Current}]}],
            Item = Instrumentation ++ lists:nth(5, Clause),
            Clausein = setelement(5, lists:nth(Current, ClauseList), Item),
            Clauselistin = [Clausein] ++ instrument_clauses(ClauseList, Length, Current + 1, CurrentBranch),
            Clauselistin;
        true ->
            Fim = [ok],
            [Fim]
    end.