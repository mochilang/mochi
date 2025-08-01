#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, indexof/2, fmt1/1, listtostring1/1, deconv/2, main/0]).

% Generated by Mochi transpiler v0.10.42 (d4a70e0e24) on 2025-07-28 04:39 UTC


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


mochi_index_of(S, Ch) when is_list(S) ->
    Char = case Ch of
        [C|_] -> C;
        <<C,_/binary>> -> C;
        C when is_integer(C) -> C;
        _ -> $\0
    end,
    case string:chr(S, Char) of
        0 -> -1;
        N -> N - 1
    end;
mochi_index_of(_, _) -> -1.

indexof(S, Ch) ->
    try
        I = 0,
        Fun = fun Fun_loop(Ch, I, S) ->
    case (I < length(S)) of
        true ->
            case (string:substr(S, I + 1, ((I + 1) - I)) == Ch) of
        true -> throw({return, I});
        _ -> ok
    end,
            I_2 = (I + 1),
            Fun_loop(Ch, I_2, S);
        _ -> {Ch, I, S}
    end
end,
{Ch, I_2, S} = Fun(Ch, I, S),
        -1
    catch {return, Ret} -> Ret end.

fmt1(X) ->
    try
        Y = (float(mochi_to_int(((X * 10) + 0.5))) / 10),
        S_2 = lists:flatten(io_lib:format("~p", [Y])),
        Dot = mochi_index_of(S_2, "."),
        case (Dot < 0) of
        true -> S_3 = (S_2 ++ ".0"),
            S_4 = S_3;
        _ -> S_4 = S_2
    end,
        S_4
    catch {return, Ret} -> Ret end.

listtostring1(Xs) ->
    try
        S_5 = "[",
        I_3 = 0,
        Fun_2 = fun Fun_2_loop(I_3, S_5, Xs) ->
    case (I_3 < length(Xs)) of
        true ->
            S_6 = (S_5 ++ fmt1(lists:nth(I_3 + 1, Xs))),
            case (I_3 < (length(Xs) - 1)) of
        true -> S_7 = (S_6 ++ " "),
            S_8 = S_7;
        _ -> S_8 = S_6
    end,
            I_4 = (I_3 + 1),
            Fun_2_loop(I_4, S_8, Xs);
        _ -> {I_3, S_5, Xs}
    end
end,
{I_4, S_8, Xs} = Fun_2(I_3, S_5, Xs),
        (S_8 ++ "]")
    catch {return, Ret} -> Ret end.

deconv(G, F) ->
    try
        Out = [],
        I_5 = 0,
        Fun_4 = fun Fun_4_loop(F, G, I_5, Out) ->
    case (I_5 =< (length(G) - length(F))) of
        true ->
            Sum = lists:nth(I_5 + 1, G),
            J = 1,
            Fun_3 = fun Fun_3_loop(F, G, I_5, J, Out, Sum) ->
    case (J < length(F)) of
        true ->
            case (J =< I_5) of
        true -> Sum_2 = (Sum - (lists:nth((I_5 - J) + 1, Out) * lists:nth(J + 1, F))),
            Sum_3 = Sum_2;
        _ -> Sum_3 = Sum
    end,
            J_2 = (J + 1),
            Fun_3_loop(F, G, I_5, J_2, Out, Sum_3);
        _ -> {F, G, I_5, J, Out, Sum}
    end
end,
{F, G, I_5, J_2, Out, Sum_3} = Fun_3(F, G, I_5, J, Out, Sum),
            Out_2 = lists:append(Out, [(Sum_3 / lists:nth(0 + 1, F))]),
            I_6 = (I_5 + 1),
            Fun_4_loop(F, G, I_6, Out_2);
        _ -> {F, G, I_5, Out}
    end
end,
{F, G, I_6, Out_2} = Fun_4(F, G, I_5, Out),
        Out_2
    catch {return, Ret} -> Ret end.

main() ->
    try
        H = [-8, -9, -3, -1, -6, 7],
        F_2 = [-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1],
        G_2 = [24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96, 96, 31, 55, 36, 29, -43, -7],
        io:format("~ts~n", [listtostring1(H)]),
        io:format("~ts~n", [listtostring1(deconv(G_2, F_2))]),
        io:format("~ts~n", [listtostring1(F_2)]),
        io:format("~ts~n", [listtostring1(deconv(G_2, H))]),
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
