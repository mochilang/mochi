#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Tokens = string:tokens(binary_to_list(Input), " \n\r\t"),
    case Tokens of
        [] -> ok;
        [T0 | Rest] ->
            T = list_to_integer(T0),
            io:put_chars(run(T, Rest, []))
    end.

run(0, _, Out) -> string:join(lists:reverse(Out), "\n");
run(T, [N0 | Rest], Out) ->
    N = list_to_integer(N0),
    {Vals, Tail} = take_n(N, Rest, []),
    Ans = integer_to_list(solve(Vals)),
    run(T - 1, Tail, [Ans | Out]).

take_n(0, Rest, Acc) -> {lists:reverse(Acc), Rest};
take_n(N, [X | Rest], Acc) -> take_n(N - 1, Rest, [list_to_integer(X) | Acc]).

solve(Vals) ->
    T = list_to_tuple(Vals),
    outer(0, tuple_size(T), T, 0).

outer(I, N, _T, Best) when I >= N -> Best;
outer(I, N, T, Best) ->
    outer(I + 1, N, T, inner(I, I, N, T, element(I + 1, T), Best)).

inner(_I, J, N, _T, _Mn, Best) when J >= N -> Best;
inner(I, J, N, T, Mn, Best) ->
    V = element(J + 1, T),
    Mn2 = if V < Mn -> V; true -> Mn end,
    Area = Mn2 * (J - I + 1),
    Best2 = if Area > Best -> Area; true -> Best end,
    inner(I, J + 1, N, T, Mn2, Best2).
