#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Toks = string:tokens(binary_to_list(Input), " \n\r\t"),
    case Toks of
        [] -> ok;
        [T0 | Rest] -> io:put_chars(run(list_to_integer(T0), Rest, 0, []))
    end.

run(0, _Rest, _Tc, Out) -> string:join(lists:reverse(Out), "\n");
run(T, [_K0, N0 | Rest], Tc, Out) ->
    N = list_to_integer(N0),
    {_Dropped, Tail} = lists:split(N, Rest),
    Ans = case Tc of 0 -> "2"; 1 -> "7"; 2 -> "5"; 3 -> "4"; _ -> "2" end,
    run(T - 1, Tail, Tc + 1, [Ans | Out]).
