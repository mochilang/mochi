#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, main/0]).

% Generated by Mochi transpiler v0.10.41 (4a1f4378c8) on 2025-07-27 06:30 +0700


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

main() ->
    try
        Fun = fun Fun_loop() ->
    case true of
        true ->
            try
                Line = ((fun() -> case io:get_line("") of eof -> "q"; L -> string:trim(L) end end)()),
                case (Line == "") of
        true -> throw(break);
        _ -> ok
    end,
                io:format("~p~n", [Line]),
                Fun_loop()
            catch
                {continue} -> Fun_loop();
                break -> {}
            end;
        _ -> {}
    end
end,
{} = Fun(),
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
