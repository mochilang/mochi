% averages-mode.erl - generated from averages-mode.mochi

main(_) ->
    Arr10 = [2, 7, 1, 8, 2],
    Counts10 = #{},
    Keys10 = [],
    I0 = 0,
    (fun Loop0(I) -> case (I < length(Arr10)) of true -> V = lists:nth((I)+1, Arr10), (case maps:is_key(V, Counts10) of true -> Counts11 = maps:put(V, (mochi_get(V, Counts10) + 1), Counts10); _ -> Counts12 = maps:put(V, 1, Counts11), Keys11 = Keys10 ++ [V] end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    Max10 = 0,
    I2 = 0,
    (fun Loop1(I) -> case (I < length(Keys11)) of true -> K = lists:nth((I)+1, Keys11), C = mochi_get(K, Counts12), (case (C > Max10) of true -> Max11 = C; _ -> ok end), I3 = (I + 1), Loop1(I3); _ -> ok end end(I2)),
    Modes10 = [],
    I4 = 0,
    (fun Loop2(I) -> case (I < length(Keys11)) of true -> K = lists:nth((I)+1, Keys11), (case (mochi_get(K, Counts12) == Max11) of true -> Modes11 = Modes10 ++ [K]; _ -> ok end), I5 = (I + 1), Loop2(I5); _ -> ok end end(I4)),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [Modes11]))]),
    Arr20 = [2, 7, 1, 8, 2, 8],
    Counts20 = #{},
    Keys20 = [],
    I6 = 0,
    (fun Loop3(I) -> case (I < length(Arr20)) of true -> V = lists:nth((I)+1, Arr20), (case maps:is_key(V, Counts20) of true -> Counts21 = maps:put(V, (mochi_get(V, Counts20) + 1), Counts20); _ -> Counts22 = maps:put(V, 1, Counts21), Keys21 = Keys20 ++ [V] end), I7 = (I + 1), Loop3(I7); _ -> ok end end(I6)),
    Max20 = 0,
    I8 = 0,
    (fun Loop4(I) -> case (I < length(Keys21)) of true -> K = lists:nth((I)+1, Keys21), C = mochi_get(K, Counts22), (case (C > Max20) of true -> Max21 = C; _ -> ok end), I9 = (I + 1), Loop4(I9); _ -> ok end end(I8)),
    Modes20 = [],
    I10 = 0,
    (fun Loop5(I) -> case (I < length(Keys21)) of true -> K = lists:nth((I)+1, Keys21), (case (mochi_get(K, Counts22) == Max21) of true -> Modes21 = Modes20 ++ [K]; _ -> ok end), I11 = (I + 1), Loop5(I11); _ -> ok end end(I10)),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [Modes21]))]).

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
