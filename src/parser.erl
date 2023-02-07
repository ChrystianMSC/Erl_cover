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

branch_filter(Parsed, Lenght, CurrentBranch) ->
    case CurrentBranch =< Lenght of
        true ->
            Statement = lists:nth(1, lists:nth(CurrentBranch, Parsed)),
            case Statement of
                'if' ->
                    branch_filter(Parsed, Lenght, CurrentBranch + 1);
                'case' ->
                    branch_filter(Parsed, Lenght, CurrentBranch + 1);
                _ ->
                    NewParsed = erlang:delete_element(CurrentBranch, Parsed),
                    NewLenght = length(NewParsed),
                    NewCurrentBranch = 
                        case CurrentBranch == 1 of
                            true -> CurrentBranch;
                            false -> CurrentBranch - 1
                        end,
                    branch_filter(NewParsed, NewLenght, NewCurrentBranch)
            end;
        false ->
            {Parsed, Lenght}
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
instrument_parsed(Parsed, Length, CurrentBranch, Holder) ->
  case CurrentBranch =< Length of
    true ->
      InstrumentingParsed = Holder ++ [instrument_branch(Parsed, CurrentBranch)],
      instrument_parsed(Parsed, Length, CurrentBranch + 1, InstrumentingParsed);
    false ->
      Holder
  end.


instrument_branch(Parsed, CurrentBranch) ->
    Branch = lists:nth(CurrentBranch, Parsed),
    Statement = lists:nth(1, Branch),

    case Statement of
        'if' -> statement_instrument(Branch, CurrentBranch, Parsed, 3);
        'case' -> statement_instrument(Branch, CurrentBranch, Parsed, 4);
        _ -> io:fwrite("Nada ~n")
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
    case CurrentBranch of
        CurrentBranch when CurrentBranch =< ParsedLenght ->
            {Statement, Line, _} = lists:nth(CurrentBranch, Parsed),
            case Statement of
                'if' ->
                    io:format("~p/~p -> ", [Statement, Line]),
                    ClauseList = lists:nth(CurrentBranch, Parsed),
                    print_clauses(ClauseList, counters:get(persistent_term:get(1), CurrentBranch), length(ClauseList)+1, 1, 4),
                    io:fwrite("~n~n"),
                    get_result(ParsedLenght, Parsed, CurrentBranch + 1);
                'case' ->
                    io:format("~p/~p -> ", [Statement, Line]),
                    ClauseList = lists:nth(CurrentBranch, Parsed),
                    print_clauses(ClauseList, counters:get(persistent_term:get(1), CurrentBranch), length(ClauseList)+1, 1, 3),
                    io:fwrite("~n~n"),
                    get_result(ParsedLenght, Parsed, CurrentBranch + 1);
                _ ->
                    io:fwrite("Nada ~n")
            end;
        _ ->
            ok
    end.



% ------------------------------------------ PRINT CLAUSES------------------------------------------------------------

% For each clause of the if it prints if it was or not executed
print_clauses(Clauses, Num, Length, Current, OprNum) ->
    case Current of
        Num ->
            {_, Line, Opr} = lists:nth(Current, Clauses),
            io:format("~p/~p -> ", [Opr, Line]),
            print_clauses(Clauses, Num, Length, Current + 1, OprNum);
        Length ->
            ok;
        _ ->
            {_, Line} = lists:nth(Current, Clauses),
            io:fwrite("Clause not executed/~p -> ", [Line]),
            print_clauses(Clauses, Num, Length, Current + 1, OprNum)
    end.

% ------------------------------------------ PRINT CLAUSES/ ------------------------------------------------------------
