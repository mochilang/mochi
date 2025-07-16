% averages-arithmetic-mean.erl - generated from averages-arithmetic-mean.mochi

mean(V) ->
    (case (length(V) == 0) of true -> #{"ok" => false}; _ -> ok end),
    Sum0 = 0,
    I0 = 0,
    (fun Loop0(Sum, I) -> case (I < length(V)) of true -> Sum1 = (Sum + lists:nth((I)+1, V)), I1 = (I + 1), Loop0(Sum1, I1); _ -> ok end end(Sum0, I0)),
    #{"ok" => true, "mean" => (Sum1 / (length(V)))}.

main() ->
    Sets = [[], [3, 1, 4, 1, 5, 9], [1e+20, 3, 1, 4, 1, 5, 9, -1e+20], [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0.11], [10, 20, 30, 40, 50, -100, 4.7, -1100]],
    lists:foreach(fun(V) -> io:format("~p~n", ["Vector: " ++ lists:flatten(io_lib:format("~p", [V]))]), R = mean(V), (case mochi_get("ok", R) of undefined -> io:format("~p~n", ["Mean undefined"]); false -> io:format("~p~n", ["Mean undefined"]); _ -> io:format("~p~n", ["Mean of " ++ lists:flatten(io_lib:format("~p", [length(V)])) ++ " numbers is " ++ lists:flatten(io_lib:format("~p", [mochi_get("mean", R)]))]) end), io:format("~p~n", [""]) end, Sets).

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
