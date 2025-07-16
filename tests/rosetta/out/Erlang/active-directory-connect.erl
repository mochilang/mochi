% active-directory-connect.erl - generated from active-directory-connect.mochi

connect(Client) ->
    ((mochi_get(Host, Client) /= "") andalso (mochi_get(Port, Client) > 0)).

main() ->
    Client = #{"__name" => "LDAPClient", Base => "dc=example,dc=com", Host => "ldap.example.com", Port => 389, UseSSL => false, BindDN => "uid=readonlyuser,ou=People,dc=example,dc=com", BindPassword => "readonlypassword", UserFilter => "(uid=%s)", GroupFilter => "(memberUid=%s)", Attributes => ["givenName", "sn", "mail", "uid"]},
    (case connect(Client) of undefined -> io:format("~p~n", ["Failed to connect"]); false -> io:format("~p~n", ["Failed to connect"]); _ -> io:format("~p~n", ["Connected to " ++ mochi_get(Host, Client)]) end).

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
