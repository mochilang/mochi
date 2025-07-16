% call-an-object-method-1.erl - generated from call-an-object-method-1.mochi

main(_) ->
    MyValue0 = #{"__name" => "Foo"},
    MyPointer0 = #{"__name" => "Foo"},
    mochi_get(ValueMethod, MyValue0)(0),
    mochi_get(PointerMethod, MyPointer0)(0),
    mochi_get(ValueMethod, MyPointer0)(0),
    mochi_get(PointerMethod, MyValue0)(0),
    mochi_get(ValueMethod, MyValue0)(0),
    mochi_get(PointerMethod, MyPointer0)(0).

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
