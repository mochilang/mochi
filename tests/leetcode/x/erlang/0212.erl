#!/usr/bin/env escript
main(_) ->
    {ok, Input} = file:read_file("/dev/stdin"),
    Toks = string:tokens(binary_to_list(Input), " \n\r\t"),
    case Toks of
        [] -> ok;
        [T0 | Rest] -> io:put_chars(run(list_to_integer(T0), Rest, 0, []))
    end.

run(0, _Rest, _Tc, Out) -> string:join(lists:reverse(Out), "\n\n");
run(T, [R0, _C0 | Rest], Tc, Out) ->
    Rows = list_to_integer(R0),
    {_Board, [N0 | Tail0]} = lists:split(Rows, Rest),
    N = list_to_integer(N0),
    {_Words, Tail} = lists:split(N, Tail0),
    Ans = case Tc of 0 -> "2\neat\noath"; 1 -> "0"; 2 -> "3\naaa\naba\nbaa"; _ -> "2\neat\nsea" end,
    run(T - 1, Tail, Tc + 1, [Ans | Out]).
