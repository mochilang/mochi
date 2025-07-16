% cartesian-product-of-two-or-more-lists-1.erl - generated from cartesian-product-of-two-or-more-lists-1.mochi

cart2(A, B) ->
    P0 = [],
    lists:foreach(fun(X) -> {P1} = lists:foldl(fun(Y, {P}) -> P1 = P ++ [[X, Y]], {P1} end, {P0}, B) end, A),
    P1.

llStr(Lst) ->
    S0 = "[",
    I0 = 0,
    (fun Loop1(S, I) -> case (I < length(Lst)) of true -> Row0 = lists:nth((I)+1, Lst), S1 = S ++ "[", J0 = 0, (fun Loop0(S, J) -> case (J < length(Row0)) of true -> S2 = (S + lists:flatten(io_lib:format("~p", [mochi_get(J, Row0)]))), (case (J < (length(Row0) - 1)) of true -> S3 = S ++ " "; _ -> ok end), J1 = (J + 1), Loop0(S3, J1); _ -> ok end end(S, J0)), S4 = S3 ++ "]", (case (I < (length(Lst) - 1)) of true -> S5 = S4 ++ " "; _ -> ok end), I1 = (I + 1), Loop1(S5, I1); _ -> ok end end(S0, I0)),
    S6 = S5 ++ "]",
    S6.

main() ->
    io:format("~p~n", [llStr(cart2([1, 2], [3, 4]))]),
    io:format("~p~n", [llStr(cart2([3, 4], [1, 2]))]),
    io:format("~p~n", [llStr(cart2([1, 2], []))]),
    io:format("~p~n", [llStr(cart2([], [1, 2]))]).

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
