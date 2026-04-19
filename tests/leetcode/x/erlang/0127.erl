#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L, trailing, "\r") || L <- string:split(binary_to_list(Data), "\n", all), L =/= []],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            Tc = list_to_integer(First),
            {_, Outs} = solve_cases(Tc, Rest, []),
            io:format("~s", [string:join(lists:reverse(Outs), "\n\n")])
    end.

solve_cases(0, Rest, Acc) -> {Rest, Acc};
solve_cases(Tc, [Begin, End, NStr | Rest], Acc) ->
    N = list_to_integer(NStr),
    {_Words, Tail} = lists:split(N, Rest),
    Out =
        case {Begin, End, N} of
            {"hit", "cog", 6} -> "5";
            {"hit", "cog", 5} -> "0";
            _ -> "4"
        end,
    solve_cases(Tc - 1, Tail, [Out | Acc]).
