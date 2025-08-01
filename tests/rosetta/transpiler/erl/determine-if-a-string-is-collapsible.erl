#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, collapse/1, main/0]).

% Generated by Mochi transpiler v0.10.42 (a75ed5072b) on 2025-07-28 04:45 UTC


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

collapse(S) ->
    try
        I = 0,
        Prev = "",
        Res = "",
        Orig = length(S),
        Fun = fun Fun_loop(I, Orig, Prev, Res, S) ->
    case (I < length(S)) of
        true ->
            Ch = string:substr(S, I + 1, ((I + 1) - I)),
            case (Ch /= Prev) of
        true -> Res_2 = (Res ++ Ch),
            Prev_2 = Ch,
            Prev_3 = Prev_2,
            Res_3 = Res_2;
        _ -> Prev_3 = Prev,
            Res_3 = Res
    end,
            I_2 = (I + 1),
            Fun_loop(I_2, Orig, Prev_3, Res_3, S);
        _ -> {I, Orig, Prev, Res, S}
    end
end,
{I_2, Orig, Prev_3, Res_3, S} = Fun(I, Orig, Prev, Res, S),
        [Res_3, Orig, length(Res_3)]
    catch {return, Ret} -> Ret end.

main() ->
    try
        Strings = ["", "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln ", "..111111111111111111111111111111111111111111111111111111111111111777888", "I never give 'em hell, I just tell the truth, and they think it's hell. ", "                                                   ---  Harry S Truman ", "The better the 4-wheel drive, the further you'll be from help when ya get stuck!", "headmistressship", "aardvark", "😍😀🙌💃😍😍😍🙌"],
        Idx = 0,
        Fun_2 = fun Fun_2_loop(Idx, Strings) ->
    case (Idx < length(Strings)) of
        true ->
            S_2 = lists:nth(Idx + 1, Strings),
            R = collapse(S_2),
            Cs = lists:nth(0 + 1, R),
            Olen = lists:nth(1 + 1, R),
            Clen = lists:nth(2 + 1, R),
            io:format("~ts~n", [(((("original : length = " ++ lists:flatten(io_lib:format("~p", [Olen]))) ++ ", string = «««") ++ S_2) ++ "»»»")]),
            io:format("~ts~n", [(((("collapsed: length = " ++ lists:flatten(io_lib:format("~p", [Clen]))) ++ ", string = «««") ++ Cs) ++ "»»»\n")]),
            Idx_2 = (Idx + 1),
            Fun_2_loop(Idx_2, Strings);
        _ -> {Idx, Strings}
    end
end,
{Idx_2, Strings} = Fun_2(Idx, Strings),
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
