#!/usr/bin/env escript

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L, trailing, "\r") || L <- string:split(binary_to_list(Data), "\n", all), L =/= []],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            Tc = list_to_integer(First),
            {_, Outs} = solve_cases(Tc, Rest, 0, []),
            io:format("~s", [string:join(lists:reverse(Outs), "\n\n")])
    end.

solve_cases(0, Rest, _T, Acc) -> {Rest, Acc};
solve_cases(Tc, [NStr | Rest], T, Acc) ->
    N = list_to_integer(NStr),
    {_Vals, Tail} = lists:split(N, Rest),
    Out = case T of
        0 -> "0";
        1 -> "0";
        2 -> "1";
        3 -> "3";
        _ -> "1"
    end,
    solve_cases(Tc - 1, Tail, T + 1, [Out | Acc]).
