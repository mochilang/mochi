% cheryls-birthday.erl - generated from cheryls-birthday.mochi

monthUnique(B, List) ->
    C0 = 0,
    lists:foreach(fun(X) -> (case (mochi_get(month, X) == mochi_get(month, B)) of true -> C1 = (C0 + 1); _ -> ok end) end, List),
    (C1 == 1).

dayUnique(B, List) ->
    C2 = 0,
    lists:foreach(fun(X) -> (case (mochi_get(day, X) == mochi_get(day, B)) of true -> C3 = (C2 + 1); _ -> ok end) end, List),
    (C3 == 1).

monthWithUniqueDay(B, List) ->
    lists:foreach(fun(X) -> (case ((mochi_get(month, X) == mochi_get(month, B)) andalso dayUnique(X, List)) of true -> true; _ -> ok end) end, List),
    false.

bstr(B) ->
    Months = ["", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    lists:nth((mochi_get(month, B))+1, Months) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(day, B)])).

main(_) ->
    Choices0 = [#{"__name" => "Birthday", month => 5, day => 15}, #{"__name" => "Birthday", month => 5, day => 16}, #{"__name" => "Birthday", month => 5, day => 19}, #{"__name" => "Birthday", month => 6, day => 17}, #{"__name" => "Birthday", month => 6, day => 18}, #{"__name" => "Birthday", month => 7, day => 14}, #{"__name" => "Birthday", month => 7, day => 16}, #{"__name" => "Birthday", month => 8, day => 14}, #{"__name" => "Birthday", month => 8, day => 15}, #{"__name" => "Birthday", month => 8, day => 17}],
    Filtered0 = [],
    lists:foreach(fun(Bd) -> (case not monthUnique(Bd, Choices0) of true -> Filtered1 = Filtered0 ++ [Bd]; _ -> ok end) end, Choices0),
    Filtered20 = [],
    lists:foreach(fun(Bd) -> (case not monthWithUniqueDay(Bd, Filtered1) of true -> Filtered21 = Filtered20 ++ [Bd]; _ -> ok end) end, Filtered1),
    Filtered30 = [],
    lists:foreach(fun(Bd) -> (case dayUnique(Bd, Filtered21) of undefined -> ok; false -> ok; _ -> Filtered31 = Filtered30 ++ [Bd] end) end, Filtered21),
    Filtered40 = [],
    lists:foreach(fun(Bd) -> (case monthUnique(Bd, Filtered31) of undefined -> ok; false -> ok; _ -> Filtered41 = Filtered40 ++ [Bd] end) end, Filtered31),
    (case (length(Filtered41) == 1) of true -> io:format("~p~n", ["Cheryl's birthday is " ++ bstr(lists:nth((0)+1, Filtered41))]); _ -> io:format("~p~n", ["Something went wrong!"]) end).

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
