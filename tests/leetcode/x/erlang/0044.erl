#!/usr/bin/env escript

main(_) ->
    {ok, Bin} = file:read_file("/dev/stdin"),
    Lines = [string:trim(L, trailing, "\r") || L <- string:split(binary_to_list(Bin), "\n", all)],
    case Lines of
        [] -> ok;
        [TStr | Rest] when TStr =/= "" -> run(list_to_integer(string:trim(TStr)), Rest, []);
        _ -> ok
    end.

run(0, _, Out) ->
    io:put_chars(string:join(lists:reverse(Out), "\n"));
run(T, [NStr | Rest], Out) ->
    N = list_to_integer(string:trim(NStr)),
    {S, Rest1} =
        case N > 0 of
            true ->
                [SVal | RestS] = Rest,
                {SVal, RestS};
            false ->
                {"", Rest}
        end,
    [MStr | Rest2] = Rest1,
    M = list_to_integer(string:trim(MStr)),
    {P, Rest3} =
        case M > 0 of
            true ->
                [PVal | RestP] = Rest2,
                {PVal, RestP};
            false ->
                {"", Rest2}
        end,
    Ans = case is_match(S, P) of true -> "true"; false -> "false" end,
    run(T - 1, Rest3, [Ans | Out]).

is_match(S, P) ->
    go(S, P, 1, 1, 0, 1).

go(S, P, I, J, Star, Match) ->
    SLen = length(S),
    PLen = length(P),
    case I =< SLen of
        true ->
            case J =< PLen of
                true ->
                    PC = lists:nth(J, P),
                    SC = lists:nth(I, S),
                    case (PC =:= $?) orelse (PC =:= SC) of
                        true -> go(S, P, I + 1, J + 1, Star, Match);
                        false ->
                            case PC =:= $* of
                                true -> go(S, P, I, J + 1, J, I);
                                false ->
                                    case Star =/= 0 of
                                        true -> go(S, P, Match + 1, Star + 1, Star, Match + 1);
                                        false -> false
                                    end
                            end
                    end;
                false ->
                    case Star =/= 0 of
                        true -> go(S, P, Match + 1, Star + 1, Star, Match + 1);
                        false -> false
                    end
            end;
        false ->
            trailing(P, J)
    end.

trailing(P, J) when J > length(P) ->
    true;
trailing(P, J) ->
    case lists:nth(J, P) of
        $* -> trailing(P, J + 1);
        _ -> false
    end.
