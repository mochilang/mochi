#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1]).

% Generated by Mochi transpiler v0.10.41 (4e0e7fcd7f) on 2025-07-26 23:05 UTC


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


mochi_not(X) ->
    case X of
        true -> false;
        false -> true;
        nil -> true;
        _ -> false
    end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('nPts', 100),
    erlang:put('rMin', 10),
    erlang:put('rMax', 15),
    erlang:put('span', ((15 + 1) + 15)),
    erlang:put('rows', []),
    erlang:put('r', 0),
    Fun_2 = fun Fun_2_loop() ->
    case (erlang:get('r') < erlang:get('span')) of
        true ->
            Row = [],
            C = 0,
            Fun = fun Fun_loop(C, Row) ->
    case (C < (erlang:get('span') * 2)) of
        true ->
            Row_2 = lists:append(Row, [" "]),
            C_2 = (C + 1),
            Fun_loop(C_2, Row_2);
        _ -> {C, Row}
    end
end,
{C_2, Row_2} = Fun(C, Row),
            erlang:put('rows', lists:append(erlang:get('rows'), [Row_2])),
            erlang:put('r', (erlang:get('r') + 1)),
            Fun_2_loop();
        _ -> {}
    end
end,
{} = Fun_2(),
    erlang:put('u', 0),
    erlang:put('seen', #{}),
    erlang:put('min2', (10 * 10)),
    erlang:put('max2', (15 * 15)),
    erlang:put('n', 0),
    Fun_3 = fun Fun_3_loop() ->
    case (erlang:get('n') < 100) of
        true ->
            try
                X = ((mochi_now() rem erlang:get('span')) - 15),
                Y = ((mochi_now() rem erlang:get('span')) - 15),
                Rs = ((X * X) + (Y * Y)),
                case ((Rs < erlang:get('min2')) orelse (Rs > erlang:get('max2'))) of
        true -> throw({continue});
        _ -> ok
    end,
                erlang:put('n', (erlang:get('n') + 1)),
                Row_3 = (Y + 15),
                Col = ((X + 15) * 2),
                Rows = erlang:get('rows'),
                Tmp = lists:nth(Row_3 + 1, Rows),
                Tmp_2 = lists:sublist(Tmp, Col) ++ ["*"] ++ lists:nthtail(Col + 1, Tmp),
                Rows_2 = lists:sublist(Rows, Row_3) ++ [Tmp_2] ++ lists:nthtail(Row_3 + 1, Rows),
                erlang:put('rows', Rows_2),
                Key = ((lists:flatten(io_lib:format("~p", [Row_3])) ++ ",") ++ lists:flatten(io_lib:format("~p", [Col]))),
                case mochi_not(maps:get(Key, erlang:get('seen'), nil)) of
        true -> Seen = erlang:get('seen'),
            Seen_2 = maps:put(Key, true, Seen),
            erlang:put('seen', Seen_2),
            erlang:put('u', (erlang:get('u') + 1)),
            Seen_3 = Seen_2;
        _ -> Seen_3 = nil
    end,
                Fun_3_loop()
            catch
                {continue} -> Fun_3_loop();
                break -> {}
            end;
        _ -> {}
    end
end,
{} = Fun_3(),
    erlang:put('i', 0),
    Fun_5 = fun Fun_5_loop() ->
    case (erlang:get('i') < erlang:get('span')) of
        true ->
            Line = "",
            J = 0,
            Fun_4 = fun Fun_4_loop(J, Line) ->
    case (J < (erlang:get('span') * 2)) of
        true ->
            Line_2 = (Line ++ lists:nth(J + 1, lists:nth(erlang:get('i') + 1, erlang:get('rows')))),
            J_2 = (J + 1),
            Fun_4_loop(J_2, Line_2);
        _ -> {J, Line}
    end
end,
{J_2, Line_2} = Fun_4(J, Line),
            io:format("~ts~n", [Line_2]),
            erlang:put('i', (erlang:get('i') + 1)),
            Fun_5_loop();
        _ -> {}
    end
end,
{} = Fun_5(),
    io:format("~ts~n", [(lists:flatten(io_lib:format("~p", [erlang:get('u')])) ++ " unique points")]),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
