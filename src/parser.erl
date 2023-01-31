-module(parser).
-compile(export_all).

% Reading and parsing the code
evaluate_expression(FileName) ->
  {Parsed, _} = parser(FileName),
  Filtered = branch_filter(Parsed),
  Instrumented = instrument(Filtered),
  Result = erl_eval:exprs(Instrumented, []),
  get_result(Filtered),
  Result.

% Reading and parsing the code
parser(FileName) ->
  {ok, File} = file:read_file(FileName),
  Tokens = erl_scan:string(unicode:characters_to_list(File)),
  Parsed = erl_parse:parse_exprs(Tokens).

% Filter the code to remove what is not branches 
branch_filter({Parsed, Length}) when Length > 0 ->
  {statement, Statement, Line} = Parsed[1],
  case Statement of
    'if'    -> branch_filter({lists:delete_element(1, Parsed), Length - 1});
    'case'  -> branch_filter({lists:delete_element(1, Parsed), Length - 1});
    _       -> branch_filter({Parsed, Length - 1})
  end;
branch_filter({Parsed, _}) -> Parsed.

instrument(Parsed) ->
    Counter = counters:new(length(Parsed), [atomics]),
    persistent_term:put(1, Counter),
    instrument_parsed(Parsed, [], 1).

% Instrument each branch individually, recursively and store it in the Holder
instrument_parsed([], Acc, _) -> Acc;
instrument_parsed([Branch | Rest], Acc, Index) ->
    Instrumented = instrument_branch(Branch, Index),
    instrument_parsed(Rest, [Instrumented | Acc], Index + 1).

% Returns the Branch with its clauses insturmented
instrument_branch({statement, Statement, Line}, BranchIndex) ->
    case Statement of
        'if' -> statement_instrument({statement, Statement, Line}, 3, BranchIndex);
        'case' -> statement_instrument({statement, Statement, Line}, 4, BranchIndex);
        _ -> io:fwrite("Nada ~n")
    end.

% Instruments the clause list of a statement in a parsed branch
statement_instrument(Branch, CurrentBranch, Parsed, Statement) ->
    ClauseList = lists:nth(Statement, Branch),
    InstrumentedClauses = instrument_clauses(ClauseList, length(ClauseList)+1, 1, CurrentBranch),
    NewParsed = lists:nth(CurrentBranch, Parsed),
    NewParsed#{Statement := InstrumentedClauses},
    InstrumentedClauses.

%instruments clauses in the ClauseList by adding the given code block to each clause, returning the list of instrumented clauses.
instrument_clauses(ClauseList, Length, Current, CurrentBranch) ->
    if
        Current < Length ->
            Clause = lists:nth(Current, ClauseList),
            Line = Clause#{2},
            Instrument = [                {call, Line, {remote, Line, {atom, Line, counters}, {atom, Line, add}},                [{call, Line, {remote, Line, {atom, Line, persistent_term}, {atom, Line, get}},                [{integer, Line, 1}]}, {integer, Line, CurrentBranch}, {integer, Line, Current}]}
            ] ++ Clause#{5},
            ClauseInstrumented = Clause#{5 := Instrument},
            [ClauseInstrumented | instrument_clauses(ClauseList, Length, Current + 1, CurrentBranch)];
        true ->
            []
    end.

% Use the result of the instrumentantion and print the clauses that where executed in each branch
get_branch_result(ParsedLength, Parsed, CurrentBranch) ->
    if CurrentBranch > ParsedLength ->
        ok;
    true ->
        Branch = tuple_to_list(lists:nth(CurrentBranch, Parsed)),
        Statement = lists:nth(1, Branch),
        Line = lists:nth(2, Branch),
        case Statement of
            if ->
                print_branch_result(if, Line, Branch, 3, 4);
            case ->
                print_branch_result(case, Line, Branch, 4, 3);
            _ ->
                ok
        end,
        get_branch_result(ParsedLength, Parsed, CurrentBranch + 1)
    end.

% Prints the result of a clause execution from a given branch
print_branch_result(Statement, Line, Branch, ClauseListIndex, ClauseNumberIndex) ->
    io:format("~p/~p ->\n", [Statement, Line]),
    ClauseList = lists:nth(ClauseListIndex, Branch),
    print_clauses(ClauseList, counters:get(persistent_term:get(1), CurrentBranch), length(ClauseList)+1, 1, ClauseNumberIndex),
    io:fwrite("\n").


% For each clause of the if it prints if it was or not executed
print_clauses(Clauses, Num, Length, Current, OprNum) ->
    case Current of
      Num ->
        Opr = lists:nth(OprNum, lists:nth(Current, Clauses)),
        Line = lists:nth(2, lists:nth(Current, Clauses)),
        io:format("~p/~p -> ", [Opr, Line]),
        print_clauses(Clauses, Num, Length, Current + 1, OprNum);
      Length ->
        ok;
      _ ->
        Line = lists:nth(2, lists:nth(Current, Clauses)),
        io:fwrite("Clause not executed/~p -> ", [Line]),
        print_clauses(Clauses, Num, Length, Current + 1, OprNum)
    end.