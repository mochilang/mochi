#!/usr/bin/env escript
% while_loop.erl - generated from while_loop.mochi

main(_) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < 3) of true -> io:format("~p~n", [I]), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)).
