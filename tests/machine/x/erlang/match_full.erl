#!/usr/bin/env escript
% match_full.erl - generated from match_full.mochi

classify(N) ->
    (case N of 0 -> "zero"; 1 -> "one"; _ -> "many" end).

main(_) ->
    X = 2,
    Label = (case X of 1 -> "one"; 2 -> "two"; 3 -> "three"; _ -> "unknown" end),
    io:format("~p~n", [Label]),
    Day = "sun",
    Mood = (case Day of "mon" -> "tired"; "fri" -> "excited"; "sun" -> "relaxed"; _ -> "normal" end),
    io:format("~p~n", [Mood]),
    Ok = true,
    Status = (case Ok of true -> "confirmed"; false -> "denied" end),
    io:format("~p~n", [Status]),
    io:format("~p~n", [classify(0)]),
    io:format("~p~n", [classify(5)]).
