% checkpoint-synchronization-4.erl - generated from checkpoint-synchronization-4.mochi

main(_) ->
    NMech0 = 5,
    DetailsPerMech0 = 4,
    lists:foreach(fun(Mech) -> Id = Mech, io:format("~p~n", ["worker " ++ lists:flatten(io_lib:format("~p", [Id])) ++ " contracted to assemble " ++ lists:flatten(io_lib:format("~p", [DetailsPerMech0])) ++ " details"]), io:format("~p~n", ["worker " ++ lists:flatten(io_lib:format("~p", [Id])) ++ " enters shop"]), D0 = 0, (fun Loop0(D) -> case (D < DetailsPerMech0) of true -> io:format("~p~n", ["worker " ++ lists:flatten(io_lib:format("~p", [Id])) ++ " assembling"]), io:format("~p~n", ["worker " ++ lists:flatten(io_lib:format("~p", [Id])) ++ " completed detail"]), D1 = (D + 1), Loop0(D1); _ -> ok end end(D0)), io:format("~p~n", ["worker " ++ lists:flatten(io_lib:format("~p", [Id])) ++ " leaves shop"]), io:format("~p~n", ["mechanism " ++ lists:flatten(io_lib:format("~p", [Mech])) ++ " completed"]) end, lists:seq(1, (((NMech0 + 1)))-1)).
