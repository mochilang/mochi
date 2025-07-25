#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, fib/1, main/0]).

% Generated by Mochi transpiler v0.10.41 (cac7e4e2bd) on 2025-07-26 17:59 +0700


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

fib(N) ->
    try
        (case (N < 2) of
    true -> N;
    _ -> (fib((N - 1)) + fib((N - 2)))
end)
    catch {return, Ret} -> Ret end.

main() ->
    try
        I = -1,
        Fun = fun Fun_loop(I) ->
    case (I =< 10) of
        true ->
            case (I < 0) of
        true -> io:format("~ts~n", [(("fib(" ++ lists:flatten(io_lib:format("~p", [I]))) ++ ") returned error: negative n is forbidden")]);
        _ -> io:format("~ts~n", [((("fib(" ++ lists:flatten(io_lib:format("~p", [I]))) ++ ") = ") ++ lists:flatten(io_lib:format("~p", [fib(I)])))])
    end,
            I_2 = (I + 1),
            Fun_loop(I_2);
        _ -> {I}
    end
end,
{I_2} = Fun(I),
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
