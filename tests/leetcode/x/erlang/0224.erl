#!/usr/bin/env escript

calculate(Line) ->
    calculate(Line, 0, 0, 1, []).

calculate([], Result, Number, Sign, _) ->
    Result + Sign * Number;
calculate([Ch | Rest], Result, Number, Sign, Stack) when Ch >= $0, Ch =< $9 ->
    calculate(Rest, Result, Number * 10 + (Ch - $0), Sign, Stack);
calculate([$+ | Rest], Result, Number, Sign, Stack) ->
    calculate(Rest, Result + Sign * Number, 0, 1, Stack);
calculate([$- | Rest], Result, Number, Sign, Stack) ->
    calculate(Rest, Result + Sign * Number, 0, -1, Stack);
calculate([$( | Rest], Result, _Number, Sign, Stack) ->
    calculate(Rest, 0, 0, 1, [Sign, Result | Stack]);
calculate([$) | Rest], Result, Number, Sign, [PrevSign, PrevResult | Tail]) ->
    calculate(Rest, PrevResult + PrevSign * (Result + Sign * Number), 0, 1, Tail);
calculate([_ | Rest], Result, Number, Sign, Stack) ->
    calculate(Rest, Result, Number, Sign, Stack).

main(_) ->
    {ok, Bin} = file:read_file("/dev/stdin"),
    Lines = string:split(binary_to_list(Bin), "\n", all),
    case Lines of
        [] -> ok;
        [First | Rest] ->
            case string:trim(First) of
                "" -> ok;
                Trimmed ->
                    {T, _} = string:to_integer(Trimmed),
                    Exprs = lists:sublist([string:trim(Line, trailing, "\r") || Line <- Rest], T),
                    Output = lists:join("\n", [integer_to_list(calculate(E)) || E <- Exprs]),
                    io:put_chars(Output)
            end
    end.
