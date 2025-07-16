% checkpoint-synchronization-1.erl - generated from checkpoint-synchronization-1.mochi

main(_) ->
    PartList0 = ["A", "B", "C", "D"],
    NAssemblies0 = 3,
    lists:foreach(fun(Cycle) -> io:format("~p~n", ["begin assembly cycle " ++ lists:flatten(io_lib:format("~p", [Cycle]))]), lists:foreach(fun(P) -> io:format("~p~n", [P ++ " worker begins part"]) end, PartList0), lists:foreach(fun(P) -> io:format("~p~n", [P ++ " worker completes part"]) end, PartList0), io:format("~p~n", ["assemble.  cycle " ++ lists:flatten(io_lib:format("~p", [Cycle])) ++ " complete"]) end, lists:seq(1, (((NAssemblies0 + 1)))-1)).
