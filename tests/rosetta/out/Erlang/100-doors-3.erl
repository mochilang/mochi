#!/usr/bin/env escript
%% Generated by Mochi compiler v0.10.30 on 2025-07-19T01:03:46Z
% 100-doors-3.erl - generated from 100-doors-3.mochi

main(_) ->
    Result0 = "",
    {Result1} = lists:foldl(fun(I, {Result}) -> J0 = 1, {J1} = (fun Loop0(J) -> case ((J * J) < I) of true -> J1 = (J + 1), Loop0(J1); _ -> {J} end end)(J0), (case ((J1 * J1) == I) of true -> Result1 = Result ++ "O"; _ -> Result1 = Result ++ "-" end), {Result1} end, {Result0}, lists:seq(1, (101)-1)),
    io:format("~s~n", [Result1]).
