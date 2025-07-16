% arena-storage-pool.erl - generated from arena-storage-pool.mochi

poolPut(P, X) ->
    P ++ [X].

poolGet(P) ->
    (case (length(P) == 0) of true -> io:format("~p~n", ["pool empty"]), #{"pool" => P, "val" => 0}; _ -> ok end),
    Idx = (length(P) - 1),
    V = lists:nth((Idx)+1, P),
    P0 = lists:sublist(P, (0)+1, (Idx)-(0)),
    #{"pool" => P0, "val" => V}.

clearPool(P) ->
    [].

main() ->
    Pool0 = [],
    I0 = 1,
    J0 = 2,
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [(I0 + J0)]))]),
    Pool1 = poolPut(Pool0, I0),
    Pool2 = poolPut(Pool1, J0),
    I1 = 0,
    J1 = 0,
    Res1 = poolGet(Pool2),
    Pool3 = mochi_get("pool", Res1),
    I2 = mochi_get("val", Res1),
    Res2 = poolGet(Pool3),
    Pool4 = mochi_get("pool", Res2),
    J2 = mochi_get("val", Res2),
    I3 = 4,
    J3 = 5,
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [(I3 + J3)]))]),
    Pool5 = poolPut(Pool4, I3),
    Pool6 = poolPut(Pool5, J3),
    I4 = 0,
    J4 = 0,
    Pool7 = clearPool(Pool6),
    Res3 = poolGet(Pool7),
    Pool8 = mochi_get("pool", Res3),
    I5 = mochi_get("val", Res3),
    Res4 = poolGet(Pool8),
    Pool9 = mochi_get("pool", Res4),
    J5 = mochi_get("val", Res4),
    I6 = 7,
    J6 = 8,
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [(I6 + J6)]))]).

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
