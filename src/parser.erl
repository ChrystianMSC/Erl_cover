-module(parser).

-compile(export_all).


evaluate_expression(FileName) ->
    % Reading and parsing the code
    {Parsed, ParsedLenght} = parser(FileName),
    % Filter the code to remove what is not branches 
    {FilteredParsed, FilteredLenght} = branch_filter(Parsed, ParsedLenght, 1),
    % Instrument the code to see what gonna be executed
    InstrumentedParsed = instrument(FilteredParsed, FilteredLenght),
    % Execute the code and see the result
    {value, Result, _} = erl_eval:exprs(InstrumentedParsed, []),
    get_result(FilteredLenght, FilteredParsed, 1),
    Result.

% Reading and parsing the code
parser(FileName) ->
    {ok, File} = file:read_file(FileName),
    Expression = unicode:characters_to_list(File),
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    ParsedLenght = length(Parsed),
    {Parsed, ParsedLenght}.

% Filter the code to remove what is not branches 
branch_filter(Parsed, ParsedLenght, CurrentBranch) ->
    if
        CurrentBranch =< ParsedLenght ->
            Tipe = tuple_to_list(lists:nth(CurrentBranch, Parsed)),
            Statement = lists:nth(1, Tipe),
            if 
                Statement == 'if' ->
                    branch_filter(Parsed, ParsedLenght, CurrentBranch + 1);
                Statement == 'case' ->
                    branch_filter(Parsed, ParsedLenght, CurrentBranch + 1);
                true ->
                    Filterering = erlang:delete_element(CurrentBranch, list_to_tuple(Parsed)),
                    FilteredParsed = tuple_to_list(Filterering),
                    if
                        CurrentBranch == 1 ->
                            branch_filter(FilteredParsed, ParsedLenght - 1, CurrentBranch);
                        true ->
                            branch_filter(FilteredParsed, ParsedLenght - 1, CurrentBranch - 1)
                    end
            end;
        true ->
            FilteredLenght = length(Parsed),
            {Parsed, FilteredLenght}
    end.

instrument(Parsed, Lenght) ->
    % Create a counter where each position is a branch
    Counter = counters:new(Lenght, [atomics]),
    % Add the ccounter to the persistent_term
    persistent_term:put(1, Counter),
    % Instrumenting the code
    Holder = [],
    InstrumentedParsed = instrument_parsed(Parsed, Lenght, 1, Holder),
    InstrumentedParsed.


% Instrument each branch individually, recursively and store it in the Holder
instrument_parsed(Parsed, Lenght, CurrentBranch, Holder) ->
    if
        CurrentBranch =< Lenght ->
            InstrumentingParsed = Holder ++ [instrument_branch(Parsed, CurrentBranch)],
            instrument_parsed(Parsed, Lenght, CurrentBranch+1, InstrumentingParsed);
        true ->
            Holder
    end.


% Returns the Branch with its clauses insturmented
instrument_branch(Parsed, CurrentBranch) ->
    % transform the Parsed list into a tuple
    BranchList = list_to_tuple(Parsed),

    Branch = element(CurrentBranch, BranchList),
    Statement = element(1, Branch),

    % instrument the branch based on it statemant type
    if 
        Statement == 'if' ->
            InstrumentedgParsed = statement_instrument(Branch, CurrentBranch, Parsed, 3),
            InstrumentedgParsed;

        Statement == 'case' ->
            InstrumentedgParsed = statement_instrument(Branch, CurrentBranch, Parsed, 4),
            InstrumentedgParsed;

        true ->
            io:fwrite("Nada ~n")
    end.

statement_instrument(Branch, CurrentBranch, Parsed, Statement) ->
    ClauseList = element(Statement, Branch),
    Instrumented = instrument_clauses(ClauseList, length(ClauseList)+1, 1, CurrentBranch),

    % remove the retorned element from the last function cause its not gona be used
    Filter = erlang:delete_element(length(Instrumented), list_to_tuple(Instrumented)),
    InstrumentedClauses = tuple_to_list(Filter),

    InstrumentedgParsed = setelement(Statement, lists:nth(CurrentBranch, Parsed), InstrumentedClauses),
    InstrumentedgParsed.

instrument_clauses(ClauseList, Length, Current, CurrentBranch) ->
    if
        Current < Length ->
            Clause = tuple_to_list(lists:nth(Current, ClauseList)),
            Line = lists:nth(2, Clause),

            % adding to target code the AST of a code that adds the clause number to the counter
            Instrumentation = [{call,Line,{remote,Line,{atom,Line,counters},{atom,Line,add}},
                                [{call,Line,{remote,Line,{atom,Line,persistent_term},{atom,Line,get}},
                                    [{integer,Line,1}]},{integer,Line,CurrentBranch},{integer,Line,Current}]}],

            Instrument = Instrumentation ++ lists:nth(5, Clause),
            ClauseInstrumented = setelement(5, lists:nth(Current, ClauseList), Instrument),

            ClauselistInstrumented = [ClauseInstrumented] ++ instrument_clauses(ClauseList, Length, Current + 1, CurrentBranch),
            ClauselistInstrumented;
        true ->
            Fim = [ok],
            [Fim]
    end.


% Use the result of the instrumentantion and print the clauses that where executed in each branch
get_result(ParsedLenght, Parsed, CurrentBranch) ->
    if
        CurrentBranch =< ParsedLenght ->

            Branch = tuple_to_list(lists:nth(CurrentBranch, Parsed)),
            Statement = lists:nth(1, Branch),
            Line = lists:nth(2, Branch),

            if 
                Statement == 'if' ->
                    % Print the branch name in this case the 'if' and its line
                    io:format("~p/~p -> ", [Statement, Line]),
                    ClauseList = lists:nth(3, Branch),
                    % Print the branch clauses differentiating what was and what was not executed
                    print_clauses(ClauseList, counters:get(persistent_term:get(1),CurrentBranch), length(ClauseList)+1, 1, 4),
                    io:fwrite("~n~n"),
                    get_result(ParsedLenght, Parsed, CurrentBranch + 1);

                Statement == 'case' ->
                    % Print the branch name in this case the 'case' and its line
                    io:format("~p/~p -> ", [Statement, Line]),
                    ClauseList = lists:nth(4, Branch),
                    % Print the branch clauses differentiating what was and what was not executed
                    print_clauses(ClauseList, counters:get(persistent_term:get(1),CurrentBranch), length(ClauseList)+1, 1, 3),
                    io:fwrite("~n~n"),
                    get_result(ParsedLenght, Parsed, CurrentBranch + 1);
                true ->
                    io:fwrite("Nada ~n")
            end;
        true ->
            ok
    end.


% ------------------------------------------ PRINT CLAUSES------------------------------------------------------------

% For each clause of the if it prints if it was or not executed
print_clauses(Clauses, Num, Length, Current, OprNum) ->
    if
        Current == Num ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Opr = lists:nth(OprNum, Expr),
            Line = lists:nth(2, Expr),
            io:format("~p/~p -> ", [Opr, Line]),
            print_clauses(Clauses, Num, Length, Current + 1, OprNum);
        Current == Length ->
            ok;
        true ->
            Expr = tuple_to_list(lists:nth(Current, Clauses)),
            Line = lists:nth(2, Expr),
            io:fwrite("Clause not executed/~p -> ", [Line]),
            print_clauses(Clauses, Num, Length, Current + 1, OprNum)
    end.

% ------------------------------------------ PRINT CLAUSES/ ------------------------------------------------------------
