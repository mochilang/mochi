% associative-array-merging.erl - generated from associative-array-merging.mochi

merge(Base, Update) ->
    Result0 = #{},
    {Result1} = lists:foldl(fun(K, {Result}) -> Result1 = maps:put(K, mochi_get(K, Base), Result), {Result1} end, {Result0}, maps:to_list(Base)),
    {Result2} = lists:foldl(fun(K, {Result}) -> Result2 = maps:put(K, mochi_get(K, Update), Result), {Result2} end, {Result1}, maps:to_list(Update)),
    Result2.

main() ->
    Base = #{"name" => "Rocket Skates", "price" => 12.75, "color" => "yellow"},
    Update = #{"price" => 15.25, "color" => "red", "year" => 1974},
    Result = merge(Base, Update),
    io:format("~p~n", [Result]).

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
