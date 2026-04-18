#!/usr/bin/env escript
main(_) ->
    {ok, Bin} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L) || L <- string:split(binary_to_list(Bin), "\n", all), L =/= []],
    case Lines of
        [] -> ok;
        [TStr | Rest] ->
            T = list_to_integer(TStr),
            run(T, Rest, [])
    end.

run(0, _, Out) -> io:put_chars(string:join(lists:reverse(Out), "\n"));
run(T, [S, P | Rest], Out) ->
    R = case match_at(S, P, 1, 1) of true -> "true"; false -> "false" end,
    run(T - 1, Rest, [R | Out]).

match_at(S, P, I, J) when J > length(P) -> I > length(S);
match_at(S, P, I, J) ->
    First = (I =< length(S)) andalso ((lists:nth(J, P) =:= $.) orelse (lists:nth(I, S) =:= lists:nth(J, P))),
    case (J + 1 =< length(P)) andalso (lists:nth(J + 1, P) =:= $*) of
        true -> match_at(S, P, I, J + 2) orelse (First andalso match_at(S, P, I + 1, J));
        false -> First andalso match_at(S, P, I + 1, J + 1)
    end.
