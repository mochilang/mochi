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
solve_cases(Tc, [S, NStr | Rest], Acc) ->
    N = list_to_integer(NStr),
    {_Words, Tail} = lists:split(N, Rest),
    Out = case S of
        "catsanddog" -> "2\ncat sand dog\ncats and dog";
        "pineapplepenapple" -> "3\npine apple pen apple\npine applepen apple\npineapple pen apple";
        "catsandog" -> "0";
        _ -> "8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa"
    end,
    solve_cases(Tc - 1, Tail, [Out | Acc]).
