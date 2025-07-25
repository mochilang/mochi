#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.26 on 2025-07-16T09:59:12Z
% abundant-deficient-and-perfect-number-classifications.erl - generated from abundant-deficient-and-perfect-number-classifications.mochi

pfacSum(I) ->
    Sum0 = 0,
    P0 = 1,
    (fun Loop0(P) -> case (P =< (I / 2)) of true -> (case (rem(I, P) == 0) of true -> Sum1 = (Sum0 + P); _ -> ok end), P1 = (P + 1), Loop0(P1); _ -> ok end end(P0)),
    Sum1.

main() ->
    D0 = 0,
    A0 = 0,
    Pnum0 = 0,
    I0 = 1,
    (fun Loop1(I) -> case (I =< 20000) of true -> J = pfacSum(I), (case (J < I) of true -> D1 = (D0 + 1); _ -> ok end), (case (J == I) of true -> Pnum1 = (Pnum0 + 1); _ -> ok end), (case (J > I) of true -> A1 = (A0 + 1); _ -> ok end), I1 = (I + 1), Loop1(I1); _ -> ok end end(I0)),
    io:format("~p~n", ["There are " ++ lists:flatten(io_lib:format("~p", [D1])) ++ " deficient numbers between 1 and 20000"]),
    io:format("~p~n", ["There are " ++ lists:flatten(io_lib:format("~p", [A1])) ++ " abundant numbers  between 1 and 20000"]),
    io:format("~p~n", ["There are " ++ lists:flatten(io_lib:format("~p", [Pnum1])) ++ " perfect numbers between 1 and 20000"]).

main(_) ->
    main().
