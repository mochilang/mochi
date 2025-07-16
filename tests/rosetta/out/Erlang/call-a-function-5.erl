% call-a-function-5.erl - generated from call-a-function-5.mochi

doIt(P) ->
    B0 = 0,
    (case maps:is_key("b", P) of true -> B1 = mochi_get("b", P); _ -> ok end),
    ((mochi_get("a", P) + B1) + mochi_get("c", P)).

main() ->
    P0 = #{},
    P1 = maps:put("a", 1, P0),
    P2 = maps:put("c", 9, P1),
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [doIt(P2)]))]).

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
