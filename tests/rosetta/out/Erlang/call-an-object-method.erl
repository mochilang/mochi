% call-an-object-method.erl - generated from call-an-object-method.mochi

New() ->
    B0 = #{"__name" => "Box", Contents => "rabbit", secret => 1},
    B0.

main(_) ->
    Box0 = #{"__name" => "New"},
    mochi_get(TellSecret, Box0)().

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
