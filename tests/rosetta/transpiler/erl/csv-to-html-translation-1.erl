#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1]).

% Generated by Mochi transpiler v0.10.42 (76cc1b1198) on 2025-07-28 04:25 UTC


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

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('c', ((((("Character,Speech\n" ++ "The multitude,The messiah! Show us the messiah!\n") ++ "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") ++ "The multitude,Who are you?\n") ++ "Brians mother,I'm his mother; that's who!\n") ++ "The multitude,Behold his mother! Behold his mother!")),
    erlang:put('rows', []),
    Fun = fun Fun_loop(List) ->
    case List of
        [] -> {};
        [Line|Line_rest] ->
            erlang:put('rows', lists:append(erlang:get('rows'), [string:tokens(Line, ",")])),
            Fun_loop(Line_rest)
    end
end,
{} = Fun(string:tokens(erlang:get('c'), "\n")),
    io:format("~ts~n", ["<table>"]),
    Fun_2 = fun Fun_2_loop(List) ->
    case List of
        [] -> {};
        [Row|Row_rest] ->
            Cells = "",
            Fun_3 = fun Fun_3_loop(List, Cells, Row) ->
    case List of
        [] -> {Cells, Row};
        [Cell|Cell_rest] ->
            Cells_2 = (((Cells ++ "<td>") ++ Cell) ++ "</td>"),
            Fun_3_loop(Cell_rest, Cells_2, Row)
    end
end,
{Cells_2, Row} = Fun_3(Row, Cells, Row),
            io:format("~ts~n", [(("    <tr>" ++ Cells_2) ++ "</tr>")]),
            Fun_2_loop(Row_rest)
    end
end,
{} = Fun_2(erlang:get('rows')),
    io:format("~ts~n", ["</table>"]),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
