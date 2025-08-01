#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, pow/2, isdisarium/1, main/0]).

% Generated by Mochi transpiler v0.10.42 (34844ab2bf) on 2025-07-28 07:59 +0700


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

pow(Base, Exp) ->
    try
        Result = 1,
        I = 0,
        Fun = fun Fun_loop(Base, Exp, I, Result) ->
    case (I < Exp) of
        true ->
            Result_2 = (Result * Base),
            I_2 = (I + 1),
            Fun_loop(Base, Exp, I_2, Result_2);
        _ -> {Base, Exp, I, Result}
    end
end,
{Base, Exp, I_2, Result_2} = Fun(Base, Exp, I, Result),
        Result_2
    catch {return, Ret} -> Ret end.

isdisarium(N) ->
    try
        Digits = [],
        X = N,
        case (X == 0) of
        true -> Digits_2 = lists:append(Digits, [0]),
            Digits_3 = Digits_2;
        _ -> Digits_3 = Digits
    end,
        Fun_2 = fun Fun_2_loop(Digits_3, N, X) ->
    case (X > 0) of
        true ->
            Digits_4 = lists:append(Digits_3, [(X rem 10)]),
            X_2 = mochi_to_int((X div 10)),
            Fun_2_loop(Digits_4, N, X_2);
        _ -> {Digits_3, N, X}
    end
end,
{Digits_4, N, X_2} = Fun_2(Digits_3, N, X),
        Sum = 0,
        Pos = 1,
        I_3 = (length(Digits_4) - 1),
        Fun_3 = fun Fun_3_loop(Digits_4, I_3, N, Pos, Sum, X_2) ->
    case (I_3 >= 0) of
        true ->
            Sum_2 = (Sum + pow(lists:nth(I_3 + 1, Digits_4), Pos)),
            Pos_2 = (Pos + 1),
            I_4 = (I_3 - 1),
            Fun_3_loop(Digits_4, I_4, N, Pos_2, Sum_2, X_2);
        _ -> {Digits_4, I_3, N, Pos, Sum, X_2}
    end
end,
{Digits_4, I_4, N, Pos_2, Sum_2, X_2} = Fun_3(Digits_4, I_3, N, Pos, Sum, X_2),
        (Sum_2 == N)
    catch {return, Ret} -> Ret end.

main() ->
    try
        Count = 0,
        N_2 = 0,
        Fun_4 = fun Fun_4_loop(Count, N_2) ->
    case ((Count < 19) andalso (N_2 < 3000000)) of
        true ->
            case (isdisarium(N_2) /= nil) of
        true -> io:format("~ts~n", [lists:flatten(io_lib:format("~p", [N_2]))]),
            Count_2 = (Count + 1),
            Count_3 = Count_2;
        _ -> Count_3 = Count
    end,
            N_3 = (N_2 + 1),
            Fun_4_loop(Count_3, N_3);
        _ -> {Count, N_2}
    end
end,
{Count_3, N_3} = Fun_4(Count, N_2),
        io:format("~ts~n", [(("\nFound the first " ++ lists:flatten(io_lib:format("~p", [Count_3]))) ++ " Disarium numbers.")]),
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
