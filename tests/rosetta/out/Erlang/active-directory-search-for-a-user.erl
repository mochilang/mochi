% active-directory-search-for-a-user.erl - generated from active-directory-search-for-a-user.mochi

search_user(Directory, Username) ->
    mochi_get(Username, Directory).

main() ->
    Client = #{"Base" => "dc=example,dc=com", "Host" => "ldap.example.com", "Port" => 389, "GroupFilter" => "(memberUid=%s)"},
    Directory = #{"username" => ["admins", "users"], "john" => ["users"]},
    Groups = search_user(Directory, "username"),
    (case (length(Groups) > 0) of true -> Out0 = "Groups: [", I0 = 0, (fun Loop0(Out, I) -> case (I < length(Groups)) of true -> Out1 = Out ++ "\"" ++ mochi_get(I, Groups) ++ "\"", (case (I < (length(Groups) - 1)) of true -> Out2 = Out ++ ", "; _ -> ok end), I1 = (I + 1), Loop0(Out2, I1); _ -> ok end end(Out0, I0)), Out3 = Out2 ++ "]", io:format("~p~n", [Out3]); _ -> io:format("~p~n", ["User not found"]) end).

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
