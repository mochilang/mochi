#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, liststr/1, llstr/1, cartn/1, main/0]).

% Generated by Mochi transpiler v0.10.42 (bbaa8b9136) on 2025-07-28 00:21 +0700


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

liststr(Xs) ->
    try
        S = "[",
        I = 0,
        Fun = fun Fun_loop(I, S, Xs) ->
    case (I < length(Xs)) of
        true ->
            S_2 = (S ++ lists:flatten(io_lib:format("~p", [lists:nth(I + 1, Xs)]))),
            case (I < (length(Xs) - 1)) of
        true -> S_3 = (S_2 ++ " "),
            S_4 = S_3;
        _ -> S_4 = S_2
    end,
            I_2 = (I + 1),
            Fun_loop(I_2, S_4, Xs);
        _ -> {I, S, Xs}
    end
end,
{I_2, S_4, Xs} = Fun(I, S, Xs),
        S_5 = (S_4 ++ "]"),
        S_5
    catch {return, Ret} -> Ret end.

llstr(Lst) ->
    try
        S_6 = "[",
        I_3 = 0,
        Fun_2 = fun Fun_2_loop(I_3, Lst, S_6) ->
    case (I_3 < length(Lst)) of
        true ->
            S_7 = (S_6 ++ liststr(lists:nth(I_3 + 1, Lst))),
            case (I_3 < (length(Lst) - 1)) of
        true -> S_8 = (S_7 ++ " "),
            S_9 = S_8;
        _ -> S_9 = S_7
    end,
            I_4 = (I_3 + 1),
            Fun_2_loop(I_4, Lst, S_9);
        _ -> {I_3, Lst, S_6}
    end
end,
{I_4, Lst, S_9} = Fun_2(I_3, Lst, S_6),
        S_10 = (S_9 ++ "]"),
        S_10
    catch {return, Ret} -> Ret end.

cartn(Lists) ->
    try
        case (Lists == nil) of
        true -> throw({return, []});
        _ -> ok
    end,
        A = Lists,
        case (length(A) == 0) of
        true -> throw({return, [[]]});
        _ -> ok
    end,
        C = 1,
        Fun_3 = fun Fun_3_loop(List, A, C, Lists) ->
    case List of
        [] -> {A, C, Lists};
        [Xs_2|Xs_2_rest] ->
            C_2 = (C * length(Xs_2)),
            Fun_3_loop(Xs_2_rest, A, C_2, Lists)
    end
end,
{A, C_2, Lists} = Fun_3(A, A, C, Lists),
        case (C_2 == 0) of
        true -> throw({return, []});
        _ -> ok
    end,
        Res = [],
        Idx = [],
        Fun_4 = fun Fun_4_loop(List, A, C_2, Idx, Lists, Res) ->
    case List of
        [] -> {A, C_2, Idx, Lists, Res};
        [_|__rest] ->
            Idx_2 = lists:append(Idx, [0]),
            Fun_4_loop(__rest, A, C_2, Idx_2, Lists, Res)
    end
end,
{A, C_2, Idx_2, Lists, Res} = Fun_4(A, A, C_2, Idx, Lists, Res),
        N = length(A),
        Count = 0,
        Fun_7 = fun Fun_7_loop(A, C_2, Count, Idx_2, Lists, N, Res) ->
    case (Count < C_2) of
        true ->
            try
                Row = [],
                J = 0,
                Fun_5 = fun Fun_5_loop(A, C_2, Count, Idx_2, J, Lists, N, Res, Row) ->
    case (J < N) of
        true ->
            Row_2 = lists:append(Row, [lists:nth(lists:nth(J + 1, Idx_2) + 1, lists:nth(J + 1, A))]),
            J_2 = (J + 1),
            Fun_5_loop(A, C_2, Count, Idx_2, J_2, Lists, N, Res, Row_2);
        _ -> {A, C_2, Count, Idx_2, J, Lists, N, Res, Row}
    end
end,
{A, C_2, Count, Idx_2, J_2, Lists, N, Res, Row_2} = Fun_5(A, C_2, Count, Idx_2, J, Lists, N, Res, Row),
                Res_2 = lists:append(Res, [Row_2]),
                K = (N - 1),
                Fun_6 = fun Fun_6_loop(A, C_2, Count, Idx_2, J_2, K, Lists, N, Res_2, Row_2) ->
    case (K >= 0) of
        true ->
            try
                Idx_3 = lists:sublist(Idx_2, K) ++ [(lists:nth(K + 1, Idx_2) + 1)] ++ lists:nthtail(K + 1, Idx_2),
                case (lists:nth(K + 1, Idx_3) < length(lists:nth(K + 1, A))) of
        true -> throw(break);
        _ -> ok
    end,
                Idx_4 = lists:sublist(Idx_3, K) ++ [0] ++ lists:nthtail(K + 1, Idx_3),
                K_2 = (K - 1),
                Fun_6_loop(A, C_2, Count, Idx_4, J_2, K_2, Lists, N, Res_2, Row_2)
            catch
                {continue, C0, C1, C2, C3, C4, C5, C6, C7, C8, C9} -> Fun_6_loop(C0, C1, C2, C3, C4, C5, C6, C7, C8, C9);
                break -> {A, C_2, Count, Idx_2, J_2, K, Lists, N, Res_2, Row_2}
            end;
        _ -> {A, C_2, Count, Idx_2, J_2, K, Lists, N, Res_2, Row_2}
    end
end,
{A, C_2, Count, Idx_4, J_2, K_2, Lists, N, Res_2, Row_2} = Fun_6(A, C_2, Count, Idx_2, J_2, K, Lists, N, Res_2, Row_2),
                Count_2 = (Count + 1),
                Fun_7_loop(A, C_2, Count_2, Idx_4, Lists, N, Res_2)
            catch
                {continue, C0, C1, C2, C3, C4, C5, C6} -> Fun_7_loop(C0, C1, C2, C3, C4, C5, C6);
                break -> {A, C_2, Count, Idx_2, Lists, N, Res}
            end;
        _ -> {A, C_2, Count, Idx_2, Lists, N, Res}
    end
end,
{A, C_2, Count_2, Idx_4, Lists, N, Res_2} = Fun_7(A, C_2, Count, Idx_2, Lists, N, Res),
        Res_2
    catch {return, Ret} -> Ret end.

main() ->
    try
        io:format("~ts~n", [llstr(cartn([[1, 2], [3, 4]]))]),
        io:format("~ts~n", [llstr(cartn([[3, 4], [1, 2]]))]),
        io:format("~ts~n", [llstr(cartn([[1, 2], []]))]),
        io:format("~ts~n", [llstr(cartn([[], [1, 2]]))]),
        io:format("~ts~n", [""]),
        io:format("~ts~n", ["["]),
        Fun_8 = fun Fun_8_loop(List) ->
    case List of
        [] -> {};
        [P|P_rest] ->
            io:format("~ts~n", [(" " ++ liststr(P))]),
            Fun_8_loop(P_rest)
    end
end,
{} = Fun_8(cartn([[1776, 1789], [7, 12], [4, 14, 23], [0, 1]])),
        io:format("~ts~n", ["]"]),
        io:format("~ts~n", [llstr(cartn([[1, 2, 3], [30], [500, 100]]))]),
        io:format("~ts~n", [llstr(cartn([[1, 2, 3], [], [500, 100]]))]),
        io:format("~ts~n", [""]),
        io:format("~ts~n", [llstr(cartn(nil))]),
        io:format("~ts~n", [llstr(cartn([]))]),
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
