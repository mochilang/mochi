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
        case (N < 2) of
        true -> throw({return, N});
        _ -> ok
    end,
        A = 0,
        B = 1,
        I = 1,
        Fun = fun Fun_loop(A, B, I, N) ->
    case (I < N) of
        true ->
            T = (A + B),
            A_2 = B,
            B_2 = T,
            I_2 = (I + 1),
            Fun_loop(A_2, B_2, I_2, N);
        _ -> {A, B, I, N}
    end
end,
{A_2, B_2, I_2, N} = Fun(A, B, I, N),
        B_2
    catch {return, Ret} -> Ret end.

main() ->
    try
        Fun_2 = fun Fun_2_loop(List) ->
    case List of
        [] -> {};
        [I_3|I_3_rest] ->
            case (I_3 < 0) of
        true -> io:format("~ts~n", [(("fib(" ++ lists:flatten(io_lib:format("~p", [I_3]))) ++ ") returned error: negative n is forbidden")]);
        _ -> io:format("~ts~n", [((("fib(" ++ lists:flatten(io_lib:format("~p", [I_3]))) ++ ") = ") ++ lists:flatten(io_lib:format("~p", [fib(I_3)])))])
    end,
            Fun_2_loop(I_3_rest)
    end
end,
{} = Fun_2([-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
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
