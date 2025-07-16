% cartesian-product-of-two-or-more-lists-2.erl - generated from cartesian-product-of-two-or-more-lists-2.mochi

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

cartN(Lists) ->
    (case (Lists == undefined) of true -> []; _ -> ok end),
    A = Lists,
    (case (length(A) == 0) of true -> [[]]; _ -> ok end),
    C0 = 1,
    {C1} = lists:foldl(fun(Xs, {C}) -> C1 = (C * length(Xs)), {C1} end, {C0}, A),
    (case (C1 == 0) of true -> []; _ -> ok end),
    Res0 = [],
    Idx0 = [],
    {Idx1} = lists:foldl(fun(_, {Idx}) -> Idx1 = Idx ++ [0], {Idx1} end, {Idx0}, A),
    N0 = length(A),
    Count0 = 0,
    (fun Loop4(Count, Res) -> case (Count < C1) of true -> Row0 = [], J0 = 0, (fun Loop2(J, Row) -> case (J < N0) of true -> Row1 = Row ++ [mochi_get(lists:nth((J)+1, Idx1), mochi_get(J, A))], J1 = (J + 1), Loop2(Row1, J1); _ -> ok end end(J0, Row0)), Res1 = Res ++ [Row1], K0 = (N0 - 1), (fun Loop3(Idx, K) -> case (K >= 0) of true -> Idx2 = lists:sublist(Idx, K) ++ [(lists:nth((K)+1, Idx) + 1)] ++ lists:nthtail((K)+1, Idx), (case (lists:nth((K)+1, Idx) < length(mochi_get(K, A))) of true -> throw(break); _ -> ok end), Idx3 = lists:sublist(Idx, K) ++ [0] ++ lists:nthtail((K)+1, Idx), K1 = (K - 1), Loop3(Idx3, K1); _ -> ok end end(Idx1, K0)), Count1 = (Count + 1), Loop4(Count1, Res1); _ -> ok end end(Count0, Res0)),
    Res1.

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
