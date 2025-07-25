#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, pfacsum/1, main/0]).

% Generated by Mochi transpiler v0.10.40 (2d48801c25) on 2025-07-25 19:52 +0700


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

pfacsum(I) ->
    try
        Sum = 0,
        P = 1,
        Fun = fun Fun_loop(I, P, Sum) ->
    case (P =< (I div 2)) of
        true ->
            case ((I rem P) == 0) of
        true -> Sum_2 = (Sum + P),
            Sum_3 = Sum_2;
        _ -> Sum_3 = Sum
    end,
            P_2 = (P + 1),
            Fun_loop(I, P_2, Sum_3);
        _ -> {I, P, Sum}
    end
end,
{I, P_2, Sum_3} = Fun(I, P, Sum),
        Sum_3
    catch {return, Ret} -> Ret end.

main() ->
    try
        D = 0,
        A = 0,
        Pnum = 0,
        I_2 = 1,
        Fun_2 = fun Fun_2_loop(A, D, I_2, Pnum) ->
    case (I_2 =< 20000) of
        true ->
            J = pfacsum(I_2),
            case (J < I_2) of
        true -> D_2 = (D + 1),
            D_3 = D_2;
        _ -> D_3 = D
    end,
            case (J == I_2) of
        true -> Pnum_2 = (Pnum + 1),
            Pnum_3 = Pnum_2;
        _ -> Pnum_3 = Pnum
    end,
            case (J > I_2) of
        true -> A_2 = (A + 1),
            A_3 = A_2;
        _ -> A_3 = A
    end,
            I_3 = (I_2 + 1),
            Fun_2_loop(A_3, D_3, I_3, Pnum_3);
        _ -> {A, D, I_2, Pnum}
    end
end,
{A_3, D_3, I_3, Pnum_3} = Fun_2(A, D, I_2, Pnum),
        io:format("~ts~n", [(("There are " ++ lists:flatten(io_lib:format("~p", [D_3]))) ++ " deficient numbers between 1 and 20000")]),
        io:format("~ts~n", [(("There are " ++ lists:flatten(io_lib:format("~p", [A_3]))) ++ " abundant numbers  between 1 and 20000")]),
        io:format("~ts~n", [(("There are " ++ lists:flatten(io_lib:format("~p", [Pnum_3]))) ++ " perfect numbers between 1 and 20000")]),
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
