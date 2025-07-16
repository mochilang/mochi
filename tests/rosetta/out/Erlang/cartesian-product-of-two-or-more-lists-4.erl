% cartesian-product-of-two-or-more-lists-4.erl - generated from cartesian-product-of-two-or-more-lists-4.mochi

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

copy(Xs) ->
    Out0 = [],
    {Out1} = lists:foldl(fun(V, {Out}) -> Out1 = Out ++ [V], {Out1} end, {Out0}, Xs),
    Out1.

cartN(Lists) ->
    (case (Lists == undefined) of true -> []; _ -> ok end),
    A = Lists,
    (case (length(A) == 0) of true -> [[]]; _ -> ok end),
    Out2 = [],
    Last = (length(A) - 1),
    Left = cartN(lists:sublist(A, (0)+1, (Last)-(0))),
    lists:foreach(fun(P) -> {Row1, Out3} = lists:foldl(fun(X, {Row, Out}) -> Row0 = copy(P), Row1 = Row ++ [X], Out3 = Out ++ [Row], {Row1, Out3} end, {Row, Out2}, mochi_get(Last, A)) end, Left),
    Out3.

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

mochi_get(K, M) ->
    case maps:find(K, M) of
        {ok, V} -> V;
        error ->
            Name = atom_to_list(K),
            case string:tokens(Name, "_") of
                [Pref|_] ->
                    P = list_to_atom(Pref),
                    case maps:find(P, M) of
                        {ok, Sub} when is_map(Sub) -> maps:get(K, Sub, undefined);
                        _ -> undefined
                    end;
                _ -> undefined
            end
        end.
