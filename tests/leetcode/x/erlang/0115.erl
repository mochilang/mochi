#!/usr/bin/env escript

solve(S, T) ->
    N = length(T),
    D0 = [1 | lists:duplicate(N, 0)],
    lists:last(lists:foldl(fun(Ch, DP) -> update(N, T, Ch, DP) end, D0, S)).

update(0, _T, _Ch, DP) -> DP;
update(J, T, Ch, DP) ->
    Cur = case lists:nth(J, T) =:= Ch of
        true -> set_nth(J + 1, lists:nth(J + 1, DP) + lists:nth(J, DP), DP);
        false -> DP
    end,
    update(J - 1, T, Ch, Cur).

set_nth(1, V, [_ | T]) -> [V | T];
set_nth(N, V, [H | T]) -> [H | set_nth(N - 1, V, T)].

main(_) ->
    {ok, Data} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L, trailing, "\r") || L <- string:split(binary_to_list(Data), "\n", all), L =/= []],
    case Lines of
        [] -> ok;
        [First | Rest] ->
            Tc = list_to_integer(string:trim(First)),
            Outs = [integer_to_list(solve(lists:nth(1 + 2 * I, Rest), lists:nth(2 + 2 * I, Rest))) || I <- lists:seq(0, Tc - 1)],
            io:format("~s", [string:join(Outs, "\n")])
    end.
