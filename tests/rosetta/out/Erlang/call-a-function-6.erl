% call-a-function-6.erl - generated from call-a-function-6.mochi

bar(A, B, C) ->
    io:format("~p~n", [lists:flatten(io_lib:format("~p", [A])) ++ ", " ++ lists:flatten(io_lib:format("~p", [B])) ++ ", " ++ lists:flatten(io_lib:format("~p", [C]))]).

main() ->
    Args0 = #{},
    Args1 = maps:put("a", 3, Args0),
    Args2 = maps:put("b", 2, Args1),
    Args3 = maps:put("c", 1, Args2),
    bar(mochi_get("a", Args3), mochi_get("b", Args3), mochi_get("c", Args3)).

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
