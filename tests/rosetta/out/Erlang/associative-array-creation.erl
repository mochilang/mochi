% associative-array-creation.erl - generated from associative-array-creation.mochi

removeKey(M, K) ->
    Out0 = #{},
    lists:foreach(fun({Key,_}) -> (case (Key /= K) of true -> Out1 = maps:put(Key, mochi_get(Key, M), Out0); _ -> ok end) end, maps:to_list(M)),
    Out1.

main() ->
    X0 = undefined,
    X1 = #{},
    X2 = maps:put("foo", 3, X1),
    Y1 = mochi_get("bar", X2),
    Ok = maps:is_key("bar", X2),
    io:format("~p~n", [Y1]),
    io:format("~p~n", [Ok]),
    X3 = removeKey(X2, "foo"),
    X4 = #{"foo" => 2, "bar" => 42, "baz" => -1},
    io:format("~p ~p ~p~n", [mochi_get("foo", X4), mochi_get("bar", X4), mochi_get("baz", X4)]).

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
