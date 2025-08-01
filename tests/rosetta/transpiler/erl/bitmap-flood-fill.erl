#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, flood/3]).

% Generated by Mochi transpiler v0.10.40 (e0c44791e6) on 2025-07-25 18:01 +0700


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

flood(X, Y, Repl) ->
    try
        Target = lists:nth(X + 1, lists:nth(Y + 1, erlang:get('grid'))),
        case (Target == Repl) of
        true -> throw({return, nil});
        _ -> ok
    end,
        Ff = fun Ff(Px, Py) ->
    try
        case ((((Px < 0) orelse (Py < 0)) orelse (Py >= length(erlang:get('grid')))) orelse (Px >= length(lists:nth(0 + 1, erlang:get('grid'))))) of
        true -> throw({return, nil});
        _ -> ok
    end,
        case (lists:nth(Px + 1, lists:nth(Py + 1, erlang:get('grid'))) /= Target) of
        true -> throw({return, nil});
        _ -> ok
    end,
        Grid = erlang:get('grid'),
        Tmp = lists:nth(Py + 1, Grid),
        Tmp_2 = lists:sublist(Tmp, Px) ++ [Repl] ++ lists:nthtail(Px + 1, Tmp),
        Grid_2 = lists:sublist(Grid, Py) ++ [Tmp_2] ++ lists:nthtail(Py + 1, Grid),
        erlang:put('grid', Grid_2),
        Ff((Px - 1), Py),
        Ff((Px + 1), Py),
        Ff(Px, (Py - 1)),
        Ff(Px, (Py + 1)),
        nil
    catch {return, Ret} -> Ret end
end,
        Ff(X, Y),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    Start = mochi_now(),
    StartMem = erlang:memory(total),
    erlang:put('grid', [[".", ".", ".", ".", "."], [".", "#", "#", "#", "."], [".", "#", ".", "#", "."], [".", "#", "#", "#", "."], [".", ".", ".", ".", "."]]),
    flood(2, 2, "o"),
    Fun = fun Fun_loop(List) ->
    case List of
        [] -> {};
        [Row|Row_rest] ->
            Line = "",
            Fun_2 = fun Fun_2_loop(List, Line, Row) ->
    case List of
        [] -> {Line, Row};
        [Ch|Ch_rest] ->
            Line_2 = (Line ++ Ch),
            Fun_2_loop(Ch_rest, Line_2, Row)
    end
end,
{Line_2, Row} = Fun_2(Row, Line, Row),
            io:format("~ts~n", [Line_2]),
            Fun_loop(Row_rest)
    end
end,
{} = Fun(erlang:get('grid')),
    End = mochi_now(),
    EndMem = erlang:memory(total),
    DurationUs = (End - Start) div 1000,
    MemBytes = abs(EndMem - StartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
