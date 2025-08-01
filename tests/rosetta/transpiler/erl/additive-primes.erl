#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, isprime/1, sumdigits/1, pad/1, main/0]).

% Generated by Mochi transpiler v0.10.40 (a7ef2ff7cf) on 2025-07-25 21:10 +0700


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

isprime(N) ->
    try
        case (N < 2) of
        true -> throw({return, false});
        _ -> ok
    end,
        case ((N rem 2) == 0) of
        true -> throw({return, (N == 2)});
        _ -> ok
    end,
        case ((N rem 3) == 0) of
        true -> throw({return, (N == 3)});
        _ -> ok
    end,
        D = 5,
        Fun = fun Fun_loop(D, N) ->
    case ((D * D) =< N) of
        true ->
            case ((N rem D) == 0) of
        true -> throw({return, false});
        _ -> ok
    end,
            D_2 = (D + 2),
            case ((N rem D_2) == 0) of
        true -> throw({return, false});
        _ -> ok
    end,
            D_3 = (D_2 + 4),
            Fun_loop(D_3, N);
        _ -> {D, N}
    end
end,
{D_3, N} = Fun(D, N),
        true
    catch {return, Ret} -> Ret end.

sumdigits(N_2) ->
    try
        S = 0,
        X = N_2,
        Fun_2 = fun Fun_2_loop(N_2, S, X) ->
    case (X > 0) of
        true ->
            S_2 = (S + (X rem 10)),
            X_2 = mochi_to_int((X div 10)),
            Fun_2_loop(N_2, S_2, X_2);
        _ -> {N_2, S, X}
    end
end,
{N_2, S_2, X_2} = Fun_2(N_2, S, X),
        S_2
    catch {return, Ret} -> Ret end.

pad(N_3) ->
    try
        case (N_3 < 10) of
        true -> throw({return, ("  " ++ lists:flatten(io_lib:format("~p", [N_3])))});
        _ -> ok
    end,
        case (N_3 < 100) of
        true -> throw({return, (" " ++ lists:flatten(io_lib:format("~p", [N_3])))});
        _ -> ok
    end,
        lists:flatten(io_lib:format("~p", [N_3]))
    catch {return, Ret} -> Ret end.

main() ->
    try
        io:format("~ts~n", ["Additive primes less than 500:"]),
        Count = 0,
        Line = "",
        LineCount = 0,
        I = 2,
        Fun_3 = fun Fun_3_loop(Count, I, Line, LineCount) ->
    case (I < 500) of
        true ->
            case (isprime(I) andalso isprime(sumdigits(I))) of
        true -> Count_2 = (Count + 1),
            Line_2 = ((Line ++ pad(I)) ++ "  "),
            LineCount_2 = (LineCount + 1),
            case (LineCount_2 == 10) of
        true -> io:format("~ts~n", [string:substr(Line_2, 0 + 1, ((length(Line_2) - 2) - 0))]),
            Line_3 = "",
            LineCount_3 = 0,
            Line_4 = Line_3,
            LineCount_4 = LineCount_3;
        _ -> Line_4 = Line_2,
            LineCount_4 = LineCount_2
    end,
            Count_3 = Count_2,
            Line_5 = Line_4,
            LineCount_5 = LineCount_4;
        _ -> Count_3 = Count,
            Line_5 = Line,
            LineCount_5 = LineCount
    end,
            case (I > 2) of
        true -> I_2 = (I + 2),
            I_4 = I_2;
        _ -> I_3 = (I + 1),
            I_4 = I_3
    end,
            Fun_3_loop(Count_3, I_4, Line_5, LineCount_5);
        _ -> {Count, I, Line, LineCount}
    end
end,
{Count_3, I_4, Line_5, LineCount_5} = Fun_3(Count, I, Line, LineCount),
        case (LineCount_5 > 0) of
        true -> io:format("~ts~n", [string:substr(Line_5, 0 + 1, ((length(Line_5) - 2) - 0))]);
        _ -> ok
    end,
        io:format("~ts~n", [(lists:flatten(io_lib:format("~p", [Count_3])) ++ " additive primes found.")]),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    Start = mochi_now(),
    StartMem = erlang:memory(total),
    main(),
    End = mochi_now(),
    EndMem = erlang:memory(total),
    DurationUs = (End - Start) div 1000,
    MemBytes = abs(EndMem - StartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
