#!/usr/bin/env escript
% break_continue.erl - generated from break_continue.mochi

main(_) ->
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    try lists:foreach(fun(N) -> try (if ((N rem 2) == 0) -> throw(continue); true -> ok end), (if (N > 7) -> throw(break); true -> ok end), io:format("~p ~p~n", ["odd number:", N]) catch throw:continue -> ok end end, Numbers) catch throw:break -> ok end.
