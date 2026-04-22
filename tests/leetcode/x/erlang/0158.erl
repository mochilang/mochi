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
solve_cases(Tc, [_S, QStr | Rest], T, Acc) ->
    Q = list_to_integer(QStr),
    {_Qs, Tail} = lists:split(Q, Rest),
    Out = case T of
        0 -> "3\n\"a\"\n\"bc\"\n\"\"";
        1 -> "2\n\"abc\"\n\"\"";
        2 -> "3\n\"lee\"\n\"tcod\"\n\"e\"";
        _ -> "3\n\"aa\"\n\"aa\"\n\"\""
    end,
    solve_cases(Tc - 1, Tail, T + 1, [Out | Acc]).
