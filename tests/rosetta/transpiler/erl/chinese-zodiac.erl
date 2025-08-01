#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, cz/6]).

% Generated by Mochi transpiler v0.10.41 (df7e5d4e15) on 2025-07-27 05:47 +0700


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

cz(Yr, Animal, YinYang, Element, Sc, Bc) ->
    try
        Y = (Yr - 4),
        Stem = (Y rem 10),
        Branch = (Y rem 12),
        Sb = (lists:nth(Stem + 1, Sc) ++ lists:nth(Branch + 1, Bc)),
        #{"animal" => lists:nth(Branch + 1, erlang:get('animal')), "yinYang" => lists:nth((Stem rem 2) + 1, erlang:get('yinYang')), "element" => lists:nth(mochi_to_int((Stem / 2)) + 1, erlang:get('element')), "stemBranch" => Sb, "cycle" => ((Y rem 60) + 1)}
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('animal', ["Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"]),
    erlang:put('yinYang', ["Yang", "Yin"]),
    erlang:put('element', ["Wood", "Fire", "Earth", "Metal", "Water"]),
    erlang:put('stemChArr', ["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"]),
    erlang:put('branchChArr', ["子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"]),
    Fun = fun Fun_loop(List) ->
    case List of
        [] -> {};
        [Yr_2|Yr_2_rest] ->
            R = cz(Yr_2, erlang:get('animal'), erlang:get('yinYang'), erlang:get('element'), erlang:get('stemChArr'), erlang:get('branchChArr')),
            io:format("~ts~n", [((((((((((lists:flatten(io_lib:format("~p", [Yr_2])) ++ ": ") ++ maps:get("element", R, nil)) ++ " ") ++ maps:get("animal", R, nil)) ++ ", ") ++ maps:get("yinYang", R, nil)) ++ ", Cycle year ") ++ lists:flatten(io_lib:format("~p", [maps:get("cycle", R, nil)]))) ++ " ") ++ maps:get("stemBranch", R, nil))]),
            Fun_loop(Yr_2_rest)
    end
end,
{} = Fun([1935, 1938, 1968, 1972, 1976]),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
