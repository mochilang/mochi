#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, removekey/2, main/0]).

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

removekey(M, K) ->
    try
        Out = #{},
        Fun = fun Fun_loop(List, K, M, Out) ->
    case List of
        [] -> {K, M, Out};
        [Key|Key_rest] ->
            case (Key /= K) of
        true -> Out_2 = maps:put(Key, maps:get(Key, M, nil), Out),
            Out_3 = Out_2;
        _ -> Out_3 = Out
    end,
            Fun_loop(Key_rest, K, M, Out_3)
    end
end,
{K, M, Out_3} = Fun(maps:keys(M), K, M, Out),
        Out_3
    catch {return, Ret} -> Ret end.

main() ->
    try
        X = nil,
        X_2 = #{},
        X_3 = maps:put("foo", 3, X_2),
        Y1 = maps:get("bar", X_3, nil),
        Ok = maps:is_key("bar", X_3),
        io:format("~p~n", [Y1]),
        io:format("~p~n", [Ok]),
        X_4 = removekey(X_3, "foo"),
        X_5 = #{"foo" => 2, "bar" => 42, "baz" => -1},
        io:format("~p ~p ~p~n", [maps:get("foo", X_5, nil), maps:get("bar", X_5, nil), maps:get("baz", X_5, nil)]),
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
