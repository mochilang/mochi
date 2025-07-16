% check-that-file-exists.erl - generated from check-that-file-exists.mochi

printStat(Fs, Path) ->
    (case maps:is_key(Path, Fs) of true -> (case mochi_get(Path, Fs) of undefined -> io:format("~p~n", [Path ++ " is a file"]); false -> io:format("~p~n", [Path ++ " is a file"]); _ -> io:format("~p~n", [Path ++ " is a directory"]) end); _ -> io:format("~p~n", ["stat " ++ Path ++ ": no such file or directory"]) end).

main() ->
    Fs0 = #{},
    Fs1 = maps:put("docs", true, Fs0),
    lists:foreach(fun(P) -> printStat(Fs1, P) end, ["input.txt", "/input.txt", "docs", "/docs"]).

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
