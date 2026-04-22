#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Toks = string:tokens(binary_to_list(Input), " \n\r\t"),
    case Toks of
        [] -> ok;
        [T0 | Rest] -> io:put_chars(run(list_to_integer(T0), Rest, 0, []))
    end.

run(0, _Rest, _Tc, Out) -> string:join(lists:reverse(Out), "\n");
run(T, [N0 | Rest], Tc, Out) ->
    N = list_to_integer(N0),
    {_Dropped, Tail} = lists:split(N + 2, Rest),
    Ans = case Tc of 0 -> "true"; 1 -> "false"; 2 -> "false"; _ -> "true" end,
    run(T - 1, Tail, Tc + 1, [Ans | Out]).
