% checkpoint-synchronization-2.erl - generated from checkpoint-synchronization-2.mochi

lower(Ch) ->
    I0 = 0,
    (fun Loop0(I) -> case (I < length(Upper)) of true -> (case (Ch == string:substr(Upper, (I)+1, ((I + 1))-(I))) of true -> string:substr(Lower, (I)+1, ((I + 1))-(I)); _ -> ok end), I1 = (I + 1), Loop0(I1); _ -> ok end end(I0)),
    Ch.

main(_) ->
    PartList0 = ["A", "B", "C", "D"],
    NAssemblies0 = 3,
    lists:foreach(fun(Cycle) -> io:format("~p~n", ["begin assembly cycle " ++ lists:flatten(io_lib:format("~p", [Cycle]))]), A0 = "", {A1} = lists:foldl(fun(P, {A}) -> io:format("~p~n", [P ++ " worker begins part"]), io:format("~p~n", [P ++ " worker completed " ++ lower(P)]), A1 = (A + lower(P)), {A1} end, {A0}, PartList0), io:format("~p~n", [A1 ++ " assembled.  cycle " ++ lists:flatten(io_lib:format("~p", [Cycle])) ++ " complete"]) end, lists:seq(1, (((NAssemblies0 + 1)))-1)).
