-module(parser).

-compile(export_all).

% Reading and parsing the code
evaluate_expression(FileName) ->
  {Parsed, ParsedLength} = parser(FileName),
  Filtered = branch_filter(Parsed, ParsedLength, 1),
  Instrumented = instrument(Filtered),
  Result = erl_eval:exprs(Instrumented, []),
  get_result(Filtered, 1),
  Result.

% Reading and parsing the code
parser(FileName) ->
  {ok, File} = file:read_file(FileName),
  Tokens = erl_scan:string(unicode:characters_to_list(File)),
  Parsed = erl_parse:parse_exprs(Tokens),
  {Parsed, length(Parsed)}.

% Filter the code to remove what is not branches 
branch_filter(Parsed, Len, Curr) ->
  if Curr =< Len ->
    Tipe = tuple_to_list(lists:nth(Curr, Parsed)),
    Statement = lists:nth(1, Tipe),
    case Statement of
      'if'    -> branch_filter(Parsed, Len, Curr + 1);
      'case'  -> branch_filter(Parsed, Len, Curr + 1);
      _       -> branch_filter(lists:delete_element(Curr, Parsed), Len - 1,
                 Curr == 1
                   and Curr - 1
                   or Curr)
    end;
    true -> {Parsed, length(Parsed)}
  end.

instrument(Parsed, Len) ->
    Counter = counters:new(Len, [atomics]),
    persistent_term:put(1, Counter),
    instrument_parsed(Parsed, Len, 1, []).


% Instrument each branch individually, recursively and store it in the Holder
instrument_parsed(Parsed, Len, Current, Acc) ->
  case Current > Len of
    true -> Acc;
    false -> instrument_parsed(Parsed, Len, Current+1, Acc ++ [instrument_branch(Parsed, Current)])
  end.

% Returns the Branch with its clauses insturmented
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
    InstrumentedClauses = instrument_clauses(ClauseList, length(ClauseList)+1, 1, CurrentBranch),
    InstrumentedgParsed = setelement(Statement, lists:nth(CurrentBranch, Parsed), InstrumentedClauses),
    InstrumentedClauses.

instrument_clauses(ClauseList, Length, Current, CurrentBranch) ->
    if
      Current < Length ->
        Clause = lists:nth(Current, ClauseList),
        Line = Clause#{2},
        Instrument = [
          {call, Line, {remote, Line, {atom, Line, counters}, {atom, Line, add}},
           [{call, Line, {remote, Line, {atom, Line, persistent_term}, {atom, Line, get}},
             [{integer, Line, 1}]}, {integer, Line, CurrentBranch}, {integer, Line, Current}]
          }
        ] ++ Clause#{5},
        ClauseInstrumented = Clause#{5 := Instrument},
        [ClauseInstrumented | instrument_clauses(ClauseList, Length, Current + 1, CurrentBranch)];
      true ->
        []
    end.

% Use the result of the instrumentantion and print the clauses that where executed in each branch
get_branch_result(ParsedLenght, Parsed, CurrentBranch) ->
    if CurrentBranch > ParsedLenght ->
        ok;
    true ->
        Branch = tuple_to_list(lists:nth(CurrentBranch, Parsed)),
        Statement = lists:nth(1, Branch),
        Line = lists:nth(2, Branch),
        case Statement of
            'if' ->
                print_branch_result('if', Line, Branch, 3, 4);
            'case' ->
                print_branch_result('case', Line, Branch, 4, 3);
            _ ->
                ok
        end,
        get_branch_result(ParsedLenght, Parsed, CurrentBranch + 1)
    end.

print_branch_result(Statement, Line, Branch, ClauseListIndex, ClauseNumberIndex) ->
    io:format("~p/~p -> ", [Statement, Line]),
    ClauseList = lists:nth(ClauseListIndex, Branch),
    print_clauses(ClauseList, counters:get(persistent_term:get(1), CurrentBranch), length(ClauseList)+1, 1, ClauseNumberIndex),
    io:fwrite("~n~n").


% For each clause of the if it prints if it was or not executed
print_clauses(Clauses, Num, Length, Current, OprNum) ->
    case Current of
        Num ->
            {Opr, Line} = {lists:nth(OprNum, lists:nth(Current, Clauses)), lists:nth(2, lists:nth(Current, Clauses))},
            io:format("~p/~p -> ", [Opr, Line]),
            print_clauses(Clauses, Num, Length, Current + 1, OprNum);
        Length ->
            ok;
        _ ->
            {Line} = {lists:nth(2, lists:nth(Current, Clauses))},
            io:fwrite("Clause not executed/~p -> ", [Line]),
            print_clauses(Clauses, Num, Length, Current + 1, OprNum)
    end.