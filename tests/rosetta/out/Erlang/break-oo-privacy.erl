% break-oo-privacy.erl - generated from break-oo-privacy.mochi

examineAndModify(F) ->
    io:format("~p~n", [" v: {" ++ lists:flatten(io_lib:format("~p", [mochi_get(Exported, F)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(unexported, F)])) ++ "} = {" ++ lists:flatten(io_lib:format("~p", [mochi_get(Exported, F)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(unexported, F)])) ++ "}"]),
    io:format("~p~n", ["    Idx Name       Type CanSet"]),
    io:format("~p~n", ["     0: Exported   int  true"]),
    io:format("~p~n", ["     1: unexported int  false"]),
    F0 = F#{Exported => 16},
    F1 = F0#{unexported => 44},
    io:format("~p~n", ["  modified unexported field via unsafe"]),
    F1.

anotherExample() ->
    io:format("~p~n", ["bufio.ReadByte returned error: unsafely injected error value into bufio inner workings"]).

main(_) ->
    Obj0 = #{"__name" => "Foobar", Exported => 12, unexported => 42},
    io:format("~p~n", ["obj: {" ++ lists:flatten(io_lib:format("~p", [mochi_get(Exported, Obj0)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(unexported, Obj0)])) ++ "}"]),
    Obj1 = examineAndModify(Obj0),
    io:format("~p~n", ["obj: {" ++ lists:flatten(io_lib:format("~p", [mochi_get(Exported, Obj1)])) ++ " " ++ lists:flatten(io_lib:format("~p", [mochi_get(unexported, Obj1)])) ++ "}"]),
    anotherExample().

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
