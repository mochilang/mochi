#!/usr/bin/env escript
%% Generated by Mochi compiler v0.10.27 on 2025-07-17T08:58:27Z
% if_then_else.erl - generated from if_then_else.mochi

main(_) ->
    Msg = (case (12 > 10) of true -> "yes"; _ -> "no" end),
    io:format("~p~n", [Msg]).
