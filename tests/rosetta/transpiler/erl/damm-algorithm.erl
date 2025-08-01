#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, damm/1, padleft/2, main/0]).

% Generated by Mochi transpiler v0.10.42 (91c3b0966f) on 2025-07-28 04:28 UTC


mochi_now() ->
    case erlang:get(now_seed) of
        undefined ->
            case os:getenv("MOCHI_NOW_SEED") of
                false -> erlang:system_time(nanosecond);
                S ->
                    case catch list_to_integer(S) of
                        {'EXIT', _} -> erlang:system_time(nanosecond);
                        Seed ->
                            erlang:put(now_seed, Seed),
                            mochi_now()
                    end
            end;
        Seed ->
            Seed2 = (Seed * 1664525 + 1013904223) rem 2147483647,
            erlang:put(now_seed, Seed2),
            Seed2
    end.


mochi_to_int(V) ->
    case erlang:is_integer(V) of
        true -> V;
        _ -> case erlang:is_float(V) of
            true -> trunc(V);
            _ -> list_to_integer(V)
        end
    end.

damm(S) ->
    try
        Tbl = [[0, 3, 1, 7, 5, 9, 8, 6, 4, 2], [7, 0, 9, 2, 1, 5, 4, 8, 6, 3], [4, 2, 0, 6, 8, 7, 1, 3, 5, 9], [1, 7, 5, 0, 9, 8, 3, 4, 2, 6], [6, 1, 2, 3, 0, 4, 5, 9, 7, 8], [3, 6, 7, 4, 2, 0, 9, 5, 8, 1], [5, 8, 6, 9, 7, 2, 0, 1, 3, 4], [8, 9, 4, 5, 3, 6, 2, 0, 1, 7], [9, 4, 3, 8, 6, 1, 7, 2, 0, 5], [2, 5, 8, 1, 4, 3, 6, 7, 9, 0]],
        Digits = #{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9},
        Interim = 0,
        I = 0,
        Fun = fun Fun_loop(Digits, I, Interim, S, Tbl) ->
    case (I < length(S)) of
        true ->
            Digit = mochi_to_int(maps:get(string:substr(S, I + 1, ((I + 1) - I)), Digits, nil)),
            Row = lists:nth(Interim + 1, Tbl),
            Interim_2 = lists:nth(Digit + 1, Row),
            I_2 = (I + 1),
            Fun_loop(Digits, I_2, Interim_2, S, Tbl);
        _ -> {Digits, I, Interim, S, Tbl}
    end
end,
{Digits, I_2, Interim_2, S, Tbl} = Fun(Digits, I, Interim, S, Tbl),
        (Interim_2 == 0)
    catch {return, Ret} -> Ret end.

padleft(S_2, Width) ->
    try
        Fun_2 = fun Fun_2_loop(S_2, Width) ->
    case (length(S_2) < Width) of
        true ->
            S_3 = (" " ++ S_2),
            Fun_2_loop(S_3, Width);
        _ -> {S_2, Width}
    end
end,
{S_3, Width} = Fun_2(S_2, Width),
        S_3
    catch {return, Ret} -> Ret end.

main() ->
    try
        Fun_3 = fun Fun_3_loop(List) ->
    case List of
        [] -> {};
        [S_4|S_4_rest] ->
            io:format("~ts~n", [((padleft(S_4, 6) ++ "  ") ++ lists:flatten(io_lib:format("~p", [damm(S_4)])))]),
            Fun_3_loop(S_4_rest)
    end
end,
{} = Fun_3(["5724", "5727", "112946", "112949"]),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    main(),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
