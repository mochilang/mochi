#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, sortrunes/1, deranged/2, main/0]).

% Generated by Mochi transpiler v0.10.40 (029b538ca5) on 2025-07-25 15:20 UTC


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

sortrunes(S) ->
    try
        Arr = [],
        I = 0,
        Fun = fun Fun_loop(Arr, I, S) ->
    case (I < length(S)) of
        true ->
            Arr_2 = lists:append(Arr, [string:substr(S, I + 1, ((I + 1) - I))]),
            I_2 = (I + 1),
            Fun_loop(Arr_2, I_2, S);
        _ -> {Arr, I, S}
    end
end,
{Arr_2, I_2, S} = Fun(Arr, I, S),
        N = length(Arr_2),
        M = 0,
        Fun_3 = fun Fun_3_loop(Arr_2, I_2, M, N, S) ->
    case (M < N) of
        true ->
            J = 0,
            Fun_2 = fun Fun_2_loop(Arr_2, I_2, J, M, N, S) ->
    case (J < (N - 1)) of
        true ->
            case (lists:nth(J + 1, Arr_2) > lists:nth((J + 1) + 1, Arr_2)) of
        true -> Tmp = lists:nth(J + 1, Arr_2),
            Arr_3 = lists:sublist(Arr_2, J) ++ [lists:nth((J + 1) + 1, Arr_2)] ++ lists:nthtail(J + 1, Arr_2),
            Arr_4 = lists:sublist(Arr_3, (J + 1)) ++ [Tmp] ++ lists:nthtail((J + 1) + 1, Arr_3),
            Arr_5 = Arr_4,
            Tmp_2 = Tmp;
        _ -> Arr_5 = Arr_2,
            Tmp_2 = nil
    end,
            J_2 = (J + 1),
            Fun_2_loop(Arr_5, I_2, J_2, M, N, S);
        _ -> {Arr_2, I_2, J, M, N, S}
    end
end,
{Arr_5, I_2, J_2, M, N, S} = Fun_2(Arr_2, I_2, J, M, N, S),
            M_2 = (M + 1),
            Fun_3_loop(Arr_5, I_2, M_2, N, S);
        _ -> {Arr_2, I_2, M, N, S}
    end
end,
{Arr_5, I_2, M_2, N, S} = Fun_3(Arr_2, I_2, M, N, S),
        Out = "",
        I_3 = 0,
        Fun_4 = fun Fun_4_loop(Arr_5, I_3, M_2, N, Out, S) ->
    case (I_3 < N) of
        true ->
            Out_2 = (Out ++ lists:nth(I_3 + 1, Arr_5)),
            I_4 = (I_3 + 1),
            Fun_4_loop(Arr_5, I_4, M_2, N, Out_2, S);
        _ -> {Arr_5, I_3, M_2, N, Out, S}
    end
end,
{Arr_5, I_4, M_2, N, Out_2, S} = Fun_4(Arr_5, I_3, M_2, N, Out, S),
        Out_2
    catch {return, Ret} -> Ret end.

deranged(A, B) ->
    try
        case (length(A) /= length(B)) of
        true -> throw({return, false});
        _ -> ok
    end,
        I_5 = 0,
        Fun_5 = fun Fun_5_loop(A, B, I_5) ->
    case (I_5 < length(A)) of
        true ->
            case (string:substr(A, I_5 + 1, ((I_5 + 1) - I_5)) == string:substr(B, I_5 + 1, ((I_5 + 1) - I_5))) of
        true -> throw({return, false});
        _ -> ok
    end,
            I_6 = (I_5 + 1),
            Fun_5_loop(A, B, I_6);
        _ -> {A, B, I_5}
    end
end,
{A, B, I_6} = Fun_5(A, B, I_5),
        true
    catch {return, Ret} -> Ret end.

main() ->
    try
        Words = ["constitutionalism", "misconstitutional"],
        M_3 = #{},
        BestLen = 0,
        W1 = "",
        W2 = "",
        Fun_6 = fun Fun_6_loop(List, BestLen, M_3, W1, W2, Words) ->
    case List of
        [] -> {BestLen, M_3, W1, W2, Words};
        [W|W_rest] ->
        try
            case (length(W) =< BestLen) of
        true -> throw({continue, BestLen, M_3, W1, W2, Words});
        _ -> ok
    end,
            K = sortrunes(W),
            case not maps:is_key(K, M_3) of
        true -> M_4 = maps:put(K, [W], M_3),
            throw({continue, BestLen, M_4, W1, W2, Words}),
            M_5 = M_4;
        _ -> M_5 = M_3
    end,
            Fun_7 = fun Fun_7_loop(List, BestLen, K, M_5, W, W1, W2, Words) ->
    case List of
        [] -> {BestLen, K, M_5, W, W1, W2, Words};
        [C|C_rest] ->
        try
            case deranged(W, C) of
        true -> BestLen_2 = length(W),
            W1_2 = C,
            W2_2 = W,
            throw(break),
            BestLen_3 = BestLen_2,
            W1_3 = W1_2,
            W2_3 = W2_2;
        _ -> BestLen_3 = BestLen,
            W1_3 = W1,
            W2_3 = W2
    end,
            Fun_7_loop(C_rest, BestLen_3, K, M_5, W, W1_3, W2_3, Words)
        catch
            {continue, C0, C1, C2, C3, C4, C5, C6} -> Fun_7_loop(C_rest, C0, C1, C2, C3, C4, C5, C6);
            {break, B0, B1, B2, B3, B4, B5, B6} -> {B0, B1, B2, B3, B4, B5, B6};
            break -> {BestLen, K, M_5, W, W1, W2, Words}
        end
    end
end,
{BestLen_3, K, M_5, W, W1_3, W2_3, Words} = Fun_7(maps:keys(maps:get(K, M_5, nil)), BestLen, K, M_5, W, W1, W2, Words),
            M_6 = maps:put(K, lists:append(maps:get(K, M_5, nil), [W]), M_5),
            Fun_6_loop(W_rest, BestLen_3, M_6, W1_3, W2_3, Words)
        catch
            {continue, C0, C1, C2, C3, C4} -> Fun_6_loop(W_rest, C0, C1, C2, C3, C4);
            {break, B0, B1, B2, B3, B4} -> {B0, B1, B2, B3, B4};
            break -> {BestLen, M_3, W1, W2, Words}
        end
    end
end,
{BestLen_3, M_6, W1_3, W2_3, Words} = Fun_6(Words, BestLen, M_3, W1, W2, Words),
        io:format("~ts~n", [((((W1_3 ++ " ") ++ W2_3) ++ " : Length ") ++ lists:flatten(io_lib:format("~p", [BestLen_3])))]),
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
