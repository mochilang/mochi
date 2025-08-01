#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, main/0]).

% Generated by Mochi transpiler v0.10.40 (0480f61bb0) on 2025-07-25 16:59 UTC


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
        M = #{"hello" => 13, "world" => 31, "!" => 71},
        Fun = fun Fun_loop(List, M) ->
    case List of
        [] -> {M};
        [K|K_rest] ->
            io:format("~ts~n", [((("key = " ++ K) ++ ", value = ") ++ lists:flatten(io_lib:format("~p", [maps:get(K, M, nil)])))]),
            Fun_loop(K_rest, M)
    end
end,
{M} = Fun(maps:keys(M), M),
        Fun_2 = fun Fun_2_loop(List, M) ->
    case List of
        [] -> {M};
        [K_2|K_2_rest] ->
            io:format("~ts~n", [("key = " ++ K_2)]),
            Fun_2_loop(K_2_rest, M)
    end
end,
{M} = Fun_2(maps:keys(M), M),
        Fun_3 = fun Fun_3_loop(List, M) ->
    case List of
        [] -> {M};
        [K_3|K_3_rest] ->
            io:format("~ts~n", [("value = " ++ lists:flatten(io_lib:format("~p", [maps:get(K_3, M, nil)])))]),
            Fun_3_loop(K_3_rest, M)
    end
end,
{M} = Fun_3(maps:keys(M), M),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    Start = mochi_now(),
    StartMem = erlang:memory(total),
    main(),
    End = mochi_now(),
    EndMem = erlang:memory(total),
    DurationUs = (End - Start) div 1000,
    MemBytes = erlang:abs(EndMem - StartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
