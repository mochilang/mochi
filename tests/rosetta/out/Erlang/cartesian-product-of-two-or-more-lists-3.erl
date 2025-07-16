% cartesian-product-of-two-or-more-lists-3.erl - generated from cartesian-product-of-two-or-more-lists-3.mochi

listStr(Xs) ->
    S0 = "[",
    I0 = 0,
    (fun Loop0(S, I) -> case (I < length(Xs)) of true -> S1 = (S + lists:flatten(io_lib:format("~p", [lists:nth((I)+1, Xs)]))), (case (I < (length(Xs) - 1)) of true -> S2 = S ++ " "; _ -> ok end), I1 = (I + 1), Loop0(S2, I1); _ -> ok end end(S0, I0)),
    S3 = S2 ++ "]",
    S3.

llStr(Lst) ->
    S4 = "[",
    I2 = 0,
    (fun Loop1(S, I) -> case (I < length(Lst)) of true -> S5 = (S + listStr(lists:nth((I)+1, Lst))), (case (I < (length(Lst) - 1)) of true -> S6 = S ++ " "; _ -> ok end), I3 = (I + 1), Loop1(S6, I3); _ -> ok end end(S4, I2)),
    S7 = S6 ++ "]",
    S7.

concat(A, B) ->
    Out0 = [],
    {Out1} = lists:foldl(fun(V, {Out}) -> Out1 = Out ++ [V], {Out1} end, {Out0}, A),
    {Out2} = lists:foldl(fun(V, {Out}) -> Out2 = Out ++ [V], {Out2} end, {Out1}, B),
    Out2.

cartN(Lists) ->
    (case (Lists == undefined) of true -> []; _ -> ok end),
    A = Lists,
    (case (length(A) == 0) of true -> [[]]; _ -> ok end),
    Out3 = [],
    Rest = cartN(lists:sublist(A, (1)+1, (length(A))-(1))),
    lists:foreach(fun(X) -> {Out4} = lists:foldl(fun(P, {Out}) -> Out4 = Out ++ [concat([X], P)], {Out4} end, {Out3}, Rest) end, lists:nth((0)+1, A)),
    Out4.

main() ->
    io:format("~p~n", [llStr(cartN([[1, 2], [3, 4]]))]),
    io:format("~p~n", [llStr(cartN([[3, 4], [1, 2]]))]),
    io:format("~p~n", [llStr(cartN([[1, 2], []]))]),
    io:format("~p~n", [llStr(cartN([[], [1, 2]]))]),
    io:format("~p~n", [""]),
    io:format("~p~n", ["["]),
    lists:foreach(fun(P) -> io:format("~p~n", [" " ++ listStr(P)]) end, cartN([[1776, 1789], [7, 12], [4, 14, 23], [0, 1]])),
    io:format("~p~n", ["]"]),
    io:format("~p~n", [llStr(cartN([[1, 2, 3], [30], [500, 100]]))]),
    io:format("~p~n", [llStr(cartN([[1, 2, 3], [], [500, 100]]))]),
    io:format("~p~n", [""]),
    io:format("~p~n", [llStr(cartN(undefined))]),
    io:format("~p~n", [llStr(cartN([]))]).

main(_) ->
    main().
