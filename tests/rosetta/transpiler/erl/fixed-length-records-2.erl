#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, repeat/2, trimrightspace/1, block2text/1, text2block/1]).

% Generated by Mochi transpiler v0.10.55 (67b72aa5ea) on 2025-08-02 22:23 +0700


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


mochi_repeat(S, N) when is_binary(S) ->
    binary:copy(S, mochi_to_int(N));
mochi_repeat(S, N) when is_list(S) ->
    string:copies(S, mochi_to_int(N));
mochi_repeat(_, _) -> [].

repeat(S, N) ->
    try
        Out = "",
        I = 0,
        Fun = fun Fun_loop(I, N, Out, S) ->
    case (I < N) of
        true ->
            Out_2 = (Out ++ S),
            I_2 = (I + 1),
            Fun_loop(I_2, N, Out_2, S);
        _ -> {I, N, Out, S}
    end
end,
{I_2, N, Out_2, S} = Fun(I, N, Out, S),
        Out_2
    catch {return, Ret} -> Ret end.

trimrightspace(S_2) ->
    try
        I_3 = (length(S_2) - 1),
        Fun_2 = fun Fun_2_loop(I_3, S_2) ->
    case ((I_3 >= 0) andalso (string:substr(S_2, I_3 + 1, ((I_3 + 1) - I_3)) == " ")) of
        true ->
            I_4 = (I_3 - 1),
            Fun_2_loop(I_4, S_2);
        _ -> {I_3, S_2}
    end
end,
{I_4, S_2} = Fun_2(I_3, S_2),
        string:substr(S_2, 1, ((I_4 + 1) - 0))
    catch {return, Ret} -> Ret end.

block2text(Block) ->
    try
        Out_3 = [],
        Fun_3 = fun Fun_3_loop(List, Block, Out_3) ->
    case List of
        [] -> {Block, Out_3};
        [B|B_rest] ->
            Out_4 = lists:append(Out_3, [trimrightspace(B)]),
            Fun_3_loop(B_rest, Block, Out_4)
    end
end,
{Block, Out_4} = Fun_3(Block, Block, Out_3),
        Out_4
    catch {return, Ret} -> Ret end.

text2block(Lines) ->
    try
        Out_5 = [],
        Count = 0,
        Fun_4 = fun Fun_4_loop(List, Count, Lines, Out_5) ->
    case List of
        [] -> {Count, Lines, Out_5};
        [Line|Line_rest] ->
            S_3 = Line,
            Le = length(S_3),
            case (Le > 64) of
        true -> S_4 = lists:sublist(S_3, 1, (64 - 0)),
            S_7 = S_4;
        _ -> case (Le < 64) of
        true -> S_5 = (S_3 ++ mochi_repeat(" ", (64 - Le))),
            S_6 = S_5;
        _ -> S_6 = S_3
    end,
            S_7 = S_6
    end,
            Out_6 = lists:append(Out_5, [S_7]),
            Count_2 = (Count + 1),
            Fun_4_loop(Line_rest, Count_2, Lines, Out_6)
    end
end,
{Count_2, Lines, Out_6} = Fun_4(Lines, Count, Lines, Out_5),
        case ((Count_2 rem 16) /= 0) of
        true -> Pad = (16 - (Count_2 rem 16)),
            I_5 = 0,
            Fun_5 = fun Fun_5_loop(Count_2, I_5, Lines, Out_6, Pad) ->
    case (I_5 < Pad) of
        true ->
            Out_7 = lists:append(Out_6, [mochi_repeat(" ", 64)]),
            I_6 = (I_5 + 1),
            Fun_5_loop(Count_2, I_6, Lines, Out_7, Pad);
        _ -> {Count_2, I_5, Lines, Out_6, Pad}
    end
end,
{Count_2, I_6, Lines, Out_7, Pad} = Fun_5(Count_2, I_5, Lines, Out_6, Pad),
            Fun_6 = Fun_5,
            I_7 = I_6,
            Out_8 = Out_7,
            Pad_2 = Pad;
        _ -> Fun_6 = Fun_4,
            I_7 = nil,
            Out_8 = Out_6,
            Pad_2 = nil
    end,
        Out_8
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('lines', ["alpha", "beta", "gamma"]),
    erlang:put('blocks', text2block(erlang:get('lines'))),
    erlang:put('outLines', block2text(erlang:get('blocks'))),
    Fun_7 = fun Fun_7_loop(List) ->
    case List of
        [] -> {};
        [L|L_rest] ->
            case (L /= "") of
        true -> io:format("~p~n", [L]);
        _ -> ok
    end,
            Fun_7_loop(L_rest)
    end
end,
{} = Fun_7(erlang:get('outLines')),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
