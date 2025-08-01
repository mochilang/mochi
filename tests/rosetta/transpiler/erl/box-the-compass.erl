#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, padleft/2, padright/2, indexof/2, format2/1, cpx/1, degrees2compasspoint/1]).

% Generated by Mochi transpiler v0.10.40 (83683ecfe3) on 2025-07-25 22:53 UTC


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

padleft(S, W) ->
    try
        Res = "",
        N = (W - length(S)),
        Fun = fun Fun_loop(N, Res, S, W) ->
    case (N > 0) of
        true ->
            Res_2 = (Res ++ " "),
            N_2 = (N - 1),
            Fun_loop(N_2, Res_2, S, W);
        _ -> {N, Res, S, W}
    end
end,
{N_2, Res_2, S, W} = Fun(N, Res, S, W),
        (Res_2 ++ S)
    catch {return, Ret} -> Ret end.

padright(S_2, W_2) ->
    try
        Out = S_2,
        I = length(S_2),
        Fun_2 = fun Fun_2_loop(I, Out, S_2, W_2) ->
    case (I < W_2) of
        true ->
            Out_2 = (Out ++ " "),
            I_2 = (I + 1),
            Fun_2_loop(I_2, Out_2, S_2, W_2);
        _ -> {I, Out, S_2, W_2}
    end
end,
{I_2, Out_2, S_2, W_2} = Fun_2(I, Out, S_2, W_2),
        Out_2
    catch {return, Ret} -> Ret end.

indexof(S_3, Ch) ->
    try
        I_3 = 0,
        Fun_3 = fun Fun_3_loop(Ch, I_3, S_3) ->
    case (I_3 < length(S_3)) of
        true ->
            case (string:substr(S_3, I_3 + 1, ((I_3 + 1) - I_3)) == Ch) of
        true -> throw({return, I_3});
        _ -> ok
    end,
            I_4 = (I_3 + 1),
            Fun_3_loop(Ch, I_4, S_3);
        _ -> {Ch, I_3, S_3}
    end
end,
{Ch, I_4, S_3} = Fun_3(Ch, I_3, S_3),
        -1
    catch {return, Ret} -> Ret end.

format2(F) ->
    try
        S_4 = lists:flatten(io_lib:format("~p", [F])),
        Idx = mochi_index_of(S_4, "."),
        case (Idx < 0) of
        true -> S_5 = (S_4 ++ ".00"),
            Fun_6 = nil,
            Need_2 = nil,
            S_9 = S_5;
        _ -> Need = (Idx + 3),
            case (length(S_4) > Need) of
        true -> S_6 = string:substr(S_4, 0 + 1, (Need - 0)),
            Fun_5 = nil,
            S_8 = S_6;
        _ -> Fun_4 = fun Fun_4_loop(F, Idx, Need, S_4) ->
    case (length(S_4) < Need) of
        true ->
            S_7 = (S_4 ++ "0"),
            Fun_4_loop(F, Idx, Need, S_7);
        _ -> {F, Idx, Need, S_4}
    end
end,
{F, Idx, Need, S_7} = Fun_4(F, Idx, Need, S_4),
            Fun_5 = Fun_4,
            S_8 = S_7
    end,
            Fun_6 = Fun_5,
            Need_2 = Need,
            S_9 = S_8
    end,
        S_9
    catch {return, Ret} -> Ret end.

cpx(H) ->
    try
        X = mochi_to_int(((H / 11.25) + 0.5)),
        X_2 = (X rem 32),
        case (X_2 < 0) of
        true -> X_3 = (X_2 + 32),
            X_4 = X_3;
        _ -> X_4 = X_2
    end,
        X_4
    catch {return, Ret} -> Ret end.

degrees2compasspoint(H_2) ->
    try
        lists:nth(cpx(H_2) + 1, erlang:get('compassPoint'))
    catch {return, Ret} -> Ret end.

main(_) ->
    Start = mochi_now(),
    StartMem = erlang:memory(total),
    erlang:put('compassPoint', ["North", "North by east", "North-northeast", "Northeast by north", "Northeast", "Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", "Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", "South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west"]),
    erlang:put('headings', [0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38]),
    io:format("~ts~n", ["Index  Compass point         Degree"]),
    erlang:put('i', 0),
    Fun_7 = fun Fun_7_loop() ->
    case (erlang:get('i') < length(erlang:get('headings'))) of
        true ->
            H_3 = lists:nth(erlang:get('i') + 1, erlang:get('headings')),
            Idx_2 = ((erlang:get('i') rem 32) + 1),
            Cp = degrees2compasspoint(H_3),
            io:format("~ts~n", [(((((padleft(lists:flatten(io_lib:format("~p", [Idx_2])), 4) ++ "   ") ++ padright(Cp, 19)) ++ " ") ++ format2(H_3)) ++ "°")]),
            erlang:put('i', (erlang:get('i') + 1)),
            Fun_7_loop();
        _ -> {}
    end
end,
{} = Fun_7(),
    End = mochi_now(),
    EndMem = erlang:memory(total),
    DurationUs = (End - Start) div 1000,
    MemBytes = erlang:abs(EndMem - StartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
