#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, liststr/1]).

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

liststr(Xs) ->
    try
        S = "[",
        I = 0,
        Fun = fun Fun_loop(I, S, Xs) ->
    case (I < length(Xs)) of
        true ->
            S_2 = (S ++ lists:flatten(io_lib:format("~p", [lists:nth(I + 1, Xs)]))),
            case ((I + 1) < length(Xs)) of
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

main(_) ->
    Start = mochi_now(),
    StartMem = erlang:memory(total),
    erlang:put('a', [0, 0, 0, 0, 0]),
    io:format("~ts~n", [("len(a) = " ++ lists:flatten(io_lib:format("~p", [length(erlang:get('a'))])))]),
    io:format("~ts~n", [("a = " ++ liststr(erlang:get('a')))]),
    A = erlang:get('a'),
    A_2 = lists:sublist(A, 0) ++ [3] ++ lists:nthtail(0 + 1, A),
    erlang:put('a', A_2),
    io:format("~ts~n", [("a = " ++ liststr(erlang:get('a')))]),
    io:format("~ts~n", [("a[0] = " ++ lists:flatten(io_lib:format("~p", [lists:nth(0 + 1, erlang:get('a'))])))]),
    erlang:put('s', lists:sublist(erlang:get('a'), 0 + 1, (4 - 0))),
    erlang:put('cap_s', 5),
    io:format("~ts~n", [("s = " ++ liststr(erlang:get('s')))]),
    io:format("~ts~n", [((("len(s) = " ++ lists:flatten(io_lib:format("~p", [length(erlang:get('s'))]))) ++ "  cap(s) = ") ++ lists:flatten(io_lib:format("~p", [erlang:get('cap_s')])))]),
    erlang:put('s', lists:sublist(erlang:get('a'), 0 + 1, (5 - 0))),
    io:format("~ts~n", [("s = " ++ liststr(erlang:get('s')))]),
    A_3 = erlang:get('a'),
    A_4 = lists:sublist(A_3, 0) ++ [22] ++ lists:nthtail(0 + 1, A_3),
    erlang:put('a', A_4),
    S_6 = erlang:get('s'),
    S_7 = lists:sublist(S_6, 0) ++ [22] ++ lists:nthtail(0 + 1, S_6),
    erlang:put('s', S_7),
    io:format("~ts~n", [("a = " ++ liststr(erlang:get('a')))]),
    io:format("~ts~n", [("s = " ++ liststr(erlang:get('s')))]),
    erlang:put('s', lists:append(erlang:get('s'), [4])),
    erlang:put('s', lists:append(erlang:get('s'), [5])),
    erlang:put('s', lists:append(erlang:get('s'), [6])),
    erlang:put('cap_s', 10),
    io:format("~ts~n", [("s = " ++ liststr(erlang:get('s')))]),
    io:format("~ts~n", [((("len(s) = " ++ lists:flatten(io_lib:format("~p", [length(erlang:get('s'))]))) ++ "  cap(s) = ") ++ lists:flatten(io_lib:format("~p", [erlang:get('cap_s')])))]),
    A_5 = erlang:get('a'),
    A_6 = lists:sublist(A_5, 4) ++ [-1] ++ lists:nthtail(4 + 1, A_5),
    erlang:put('a', A_6),
    io:format("~ts~n", [("a = " ++ liststr(erlang:get('a')))]),
    io:format("~ts~n", [("s = " ++ liststr(erlang:get('s')))]),
    erlang:put('s', []),
    Fun_2 = fun Fun_2_loop(List, A_6, S_7) ->
    case List of
        [] -> {A_6, S_7};
        [I_3|I_3_rest] ->
            erlang:put('s', lists:append(erlang:get('s'), [0])),
            Fun_2_loop(I_3_rest, A_6, S_7)
    end
end,
{A_6, S_7} = Fun_2(lists:seq(0, (8) - 1), A_6, S_7),
    erlang:put('cap_s', 8),
    io:format("~ts~n", [("s = " ++ liststr(erlang:get('s')))]),
    io:format("~ts~n", [((("len(s) = " ++ lists:flatten(io_lib:format("~p", [length(erlang:get('s'))]))) ++ "  cap(s) = ") ++ lists:flatten(io_lib:format("~p", [erlang:get('cap_s')])))]),
    End = mochi_now(),
    EndMem = erlang:memory(total),
    DurationUs = (End - Start) div 1000,
    MemBytes = erlang:abs(EndMem - StartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
