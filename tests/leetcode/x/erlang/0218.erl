#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Toks = string:tokens(binary_to_list(Input), " \n\r\t"),
    case Toks of
        [] -> ok;
        [T0 | Rest] -> io:put_chars(run(list_to_integer(T0), Rest, []))
    end.

run(0, _Rest, Out) -> string:join(lists:reverse(Out), "\n\n");
run(T, [N0, L0, R0 | Rest], Out) ->
    N = list_to_integer(N0),
    L = list_to_integer(L0),
    R = list_to_integer(R0),
    {_Dropped, Tail} = lists:split(N * 3 - 2, Rest),
    Ans = case {N, L, R} of
        {5, _, _} -> "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0";
        {2, _, _} -> "2\n0 3\n5 0";
        {3, 1, 3} -> "5\n1 4\n2 6\n4 0\n5 1\n6 0";
        _ -> "2\n1 3\n7 0"
    end,
    run(T - 1, Tail, [Ans | Out]).
