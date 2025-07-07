#!/usr/bin/env escript
%% match_full.erl - manual translation of tests/vm/valid/match_full.mochi

main(_) ->
    X = 2,
    Label = case X of
        1 -> "one";
        2 -> "two";
        3 -> "three";
        _ -> "unknown"
    end,
    io:format("~s~n", [Label]),

    Day = "sun",
    Mood = case Day of
        "mon" -> "tired";
        "fri" -> "excited";
        "sun" -> "relaxed";
        _ -> "normal"
    end,
    io:format("~s~n", [Mood]),

    Ok = true,
    Status = case Ok of
        true -> "confirmed";
        false -> "denied"
    end,
    io:format("~s~n", [Status]),

    Classify = fun(N) ->
        case N of
            0 -> "zero";
            1 -> "one";
            _ -> "many"
        end
    end,
    io:format("~s~n", [Classify(0)]),
    io:format("~s~n", [Classify(5)]).
