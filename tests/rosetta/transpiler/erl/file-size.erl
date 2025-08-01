#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, printsize/2, main/0]).

% Generated by Mochi transpiler v0.10.55 (6aa59f472e) on 2025-08-02 17:26 +0700


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

printsize(Fs, Path) ->
    try
        case maps:is_key(Path, Fs) of
        true -> io:format("~ts~n", [lists:flatten(io_lib:format("~p", [maps:get(Path, Fs, nil)]))]);
        _ -> io:format("~ts~n", [(("stat " ++ Path) ++ ": no such file or directory")])
    end,
        nil
    catch {return, Ret} -> Ret end.

main() ->
    try
        Fs_2 = #{},
        Fs_3 = maps:put("input.txt", 123, Fs_2),
        printsize(Fs_3, "input.txt"),
        printsize(Fs_3, "/input.txt"),
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
