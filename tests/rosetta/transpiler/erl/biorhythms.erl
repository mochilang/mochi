#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, sinapprox/1, floor/1, absfloat/1, absint/1, parseintstr/1, parsedate/1, leap/1, daysinmonth/2, adddays/4, pad2/1, datestring/3, day/3, biorhythms/2, main/0]).

% Generated by Mochi transpiler v0.10.40 (b88a2cf083) on 2025-07-25 09:44 UTC


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


mochi_pad_start(S, Len, Ch) ->
    Fill0 = case Ch of
        "" -> " ";
        _ -> Ch
    end,
    Fill = string:substr(Fill0, 1, 1),
    SL = length(S),
    case SL >= Len of
        true -> S;
        _ -> string:copies(Fill, Len - SL) ++ S
    end.

sinapprox(X) ->
    try
        Term = X,
        Sum = X,
        N = 1,
        Fun = fun Fun_loop(N, Sum, Term, X) ->
    case (N =< 8) of
        true ->
            Denom = float(((2 * N) * ((2 * N) + 1))),
            Term_2 = (((-Term * X) * X) / Denom),
            Sum_2 = (Sum + Term_2),
            N_2 = (N + 1),
            Fun_loop(N_2, Sum_2, Term_2, X);
        _ -> {N, Sum, Term, X}
    end
end,
{N_2, Sum_2, Term_2, X} = Fun(N, Sum, Term, X),
        Sum_2
    catch {return, Ret} -> Ret end.

floor(X_2) ->
    try
        I = mochi_to_int(X_2),
        case (float(I) > X_2) of
        true -> I_2 = (I - 1),
            I_3 = I_2;
        _ -> I_3 = I
    end,
        float(I_3)
    catch {return, Ret} -> Ret end.

absfloat(X_3) ->
    try
        (case (X_3 < 0) of
    true -> -X_3;
    _ -> X_3
end)
    catch {return, Ret} -> Ret end.

absint(N_3) ->
    try
        (case (N_3 < 0) of
    true -> -N_3;
    _ -> N_3
end)
    catch {return, Ret} -> Ret end.

parseintstr(Str) ->
    try
        I_4 = 0,
        Neg = false,
        case ((length(Str) > 0) andalso (string:substr(Str, 0 + 1, (1 - 0)) == "-")) of
        true -> Neg_2 = true,
            I_5 = 1,
            I_6 = I_5,
            Neg_3 = Neg_2;
        _ -> I_6 = I_4,
            Neg_3 = Neg
    end,
        N_4 = 0,
        Digits = #{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9},
        Fun_2 = fun Fun_2_loop(Digits, I_6, N_4, Neg_3, Str) ->
    case (I_6 < length(Str)) of
        true ->
            N_5 = ((N_4 * 10) + maps:get(string:substr(Str, I_6 + 1, ((I_6 + 1) - I_6)), Digits, nil)),
            I_7 = (I_6 + 1),
            Fun_2_loop(Digits, I_7, N_5, Neg_3, Str);
        _ -> {Digits, I_6, N_4, Neg_3, Str}
    end
end,
{Digits, I_7, N_5, Neg_3, Str} = Fun_2(Digits, I_6, N_4, Neg_3, Str),
        case Neg_3 of
        true -> N_6 = -N_5,
            N_7 = N_6;
        _ -> N_7 = N_5
    end,
        N_7
    catch {return, Ret} -> Ret end.

parsedate(S) ->
    try
        Y = parseintstr(string:substr(S, 0 + 1, (4 - 0))),
        M = parseintstr(string:substr(S, 5 + 1, (7 - 5))),
        D = parseintstr(string:substr(S, 8 + 1, (10 - 8))),
        [Y, M, D]
    catch {return, Ret} -> Ret end.

leap(Y_2) ->
    try
        case ((Y_2 rem 400) == 0) of
        true -> throw({return, true});
        _ -> ok
    end,
        case ((Y_2 rem 100) == 0) of
        true -> throw({return, false});
        _ -> ok
    end,
        ((Y_2 rem 4) == 0)
    catch {return, Ret} -> Ret end.

daysinmonth(Y_3, M_2) ->
    try
        Feb = (case leap(Y_3) of
    true -> 29;
    _ -> 28
end),
        Lengths = [31, Feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
        lists:nth((M_2 - 1) + 1, Lengths)
    catch {return, Ret} -> Ret end.

adddays(Y_4, M_3, D_2, N_8) ->
    try
        Yy = Y_4,
        Mm = M_3,
        Dd = D_2,
        case (N_8 >= 0) of
        true -> I_8 = 0,
            Fun_3 = fun Fun_3_loop(D_2, Dd, I_8, M_3, Mm, N_8, Y_4, Yy) ->
    case (I_8 < N_8) of
        true ->
            Dd_2 = (Dd + 1),
            case (Dd_2 > daysinmonth(Yy, Mm)) of
        true -> Dd_3 = 1,
            Mm_2 = (Mm + 1),
            case (Mm_2 > 12) of
        true -> Mm_3 = 1,
            Yy_2 = (Yy + 1),
            Mm_4 = Mm_3,
            Yy_3 = Yy_2;
        _ -> Mm_4 = Mm_2,
            Yy_3 = Yy
    end,
            Dd_4 = Dd_3,
            Mm_5 = Mm_4,
            Yy_4 = Yy_3;
        _ -> Dd_4 = Dd_2,
            Mm_5 = Mm,
            Yy_4 = Yy
    end,
            I_9 = (I_8 + 1),
            Fun_3_loop(D_2, Dd_4, I_9, M_3, Mm_5, N_8, Y_4, Yy_4);
        _ -> {D_2, Dd, I_8, M_3, Mm, N_8, Y_4, Yy}
    end
end,
{D_2, Dd_4, I_9, M_3, Mm_5, N_8, Y_4, Yy_4} = Fun_3(D_2, Dd, I_8, M_3, Mm, N_8, Y_4, Yy),
            Dd_8 = Dd_4,
            Fun_5 = Fun_3,
            I_12 = I_9,
            Mm_10 = Mm_5,
            Yy_8 = Yy_4;
        _ -> I_10 = 0,
            Fun_4 = fun Fun_4_loop(D_2, Dd, I_10, M_3, Mm, N_8, Y_4, Yy) ->
    case (I_10 > N_8) of
        true ->
            Dd_5 = (Dd - 1),
            case (Dd_5 < 1) of
        true -> Mm_6 = (Mm - 1),
            case (Mm_6 < 1) of
        true -> Mm_7 = 12,
            Yy_5 = (Yy - 1),
            Mm_8 = Mm_7,
            Yy_6 = Yy_5;
        _ -> Mm_8 = Mm_6,
            Yy_6 = Yy
    end,
            Dd_6 = daysinmonth(Yy_6, Mm_8),
            Dd_7 = Dd_6,
            Mm_9 = Mm_8,
            Yy_7 = Yy_6;
        _ -> Dd_7 = Dd_5,
            Mm_9 = Mm,
            Yy_7 = Yy
    end,
            I_11 = (I_10 - 1),
            Fun_4_loop(D_2, Dd_7, I_11, M_3, Mm_9, N_8, Y_4, Yy_7);
        _ -> {D_2, Dd, I_10, M_3, Mm, N_8, Y_4, Yy}
    end
end,
{D_2, Dd_7, I_11, M_3, Mm_9, N_8, Y_4, Yy_7} = Fun_4(D_2, Dd, I_10, M_3, Mm, N_8, Y_4, Yy),
            Dd_8 = Dd_7,
            Fun_5 = Fun_4,
            I_12 = I_11,
            Mm_10 = Mm_9,
            Yy_8 = Yy_7
    end,
        [Yy_8, Mm_10, Dd_8]
    catch {return, Ret} -> Ret end.

pad2(N_9) ->
    try
        (case (N_9 < 10) of
    true -> ("0" ++ lists:flatten(io_lib:format("~p", [N_9])));
    _ -> lists:flatten(io_lib:format("~p", [N_9]))
end)
    catch {return, Ret} -> Ret end.

datestring(Y_5, M_4, D_3) ->
    try
        ((((lists:flatten(io_lib:format("~p", [Y_5])) ++ "-") ++ pad2(M_4)) ++ "-") ++ pad2(D_3))
    catch {return, Ret} -> Ret end.

day(Y_6, M_5, D_4) ->
    try
        Part1 = (367 * Y_6),
        Part2 = mochi_to_int(((7 * mochi_to_int((Y_6 + ((M_5 + 9) div 12)))) div 4)),
        Part3 = mochi_to_int(((275 * M_5) div 9)),
        ((((Part1 - Part2) + Part3) + D_4) - 730530)
    catch {return, Ret} -> Ret end.

biorhythms(Birth, Target) ->
    try
        Bparts = parsedate(Birth),
        By = lists:nth(0 + 1, Bparts),
        Bm = lists:nth(1 + 1, Bparts),
        Bd = lists:nth(2 + 1, Bparts),
        Tparts = parsedate(Target),
        Ty = lists:nth(0 + 1, Tparts),
        Tm = lists:nth(1 + 1, Tparts),
        Td = lists:nth(2 + 1, Tparts),
        Diff = absint((day(Ty, Tm, Td) - day(By, Bm, Bd))),
        io:format("~ts~n", [((("Born " ++ Birth) ++ ", Target ") ++ Target)]),
        io:format("~ts~n", [("Day " ++ lists:flatten(io_lib:format("~p", [Diff])))]),
        Cycles = ["Physical day ", "Emotional day", "Mental day   "],
        Lengths_2 = [23, 28, 33],
        Quadrants = [["up and rising", "peak"], ["up but falling", "transition"], ["down and falling", "valley"], ["down but rising", "transition"]],
        I_13 = 0,
        Fun_6 = fun Fun_6_loop(Bd, Birth, Bm, Bparts, By, Cycles, Diff, I_13, Lengths_2, Quadrants, Target, Td, Tm, Tparts, Ty) ->
    case (I_13 < 3) of
        true ->
            Length = lists:nth(I_13 + 1, Lengths_2),
            Cycle = lists:nth(I_13 + 1, Cycles),
            Position = (Diff rem Length),
            Quadrant = ((Position * 4) div Length),
            Percent = sinapprox((((2 * 3.141592653589793) * float(Position)) / float(Length))),
            Percent_2 = (floor((Percent * 1000)) / 10),
            Description = "",
            case (Percent_2 > 95) of
        true -> Description_2 = " peak",
            DaysToAdd_4 = nil,
            Description_8 = Description_2,
            Nd_4 = nil,
            Next_4 = nil,
            Nm_4 = nil,
            Ny_4 = nil,
            Pct_6 = nil,
            Res_4 = nil,
            Transition_4 = nil,
            Trend_4 = nil;
        _ -> case (Percent_2 < -95) of
        true -> Description_3 = " valley",
            DaysToAdd_3 = nil,
            Description_7 = Description_3,
            Nd_3 = nil,
            Next_3 = nil,
            Nm_3 = nil,
            Ny_3 = nil,
            Pct_5 = nil,
            Res_3 = nil,
            Transition_3 = nil,
            Trend_3 = nil;
        _ -> case (absfloat(Percent_2) < 5) of
        true -> Description_4 = " critical transition",
            DaysToAdd_2 = nil,
            Description_6 = Description_4,
            Nd_2 = nil,
            Next_2 = nil,
            Nm_2 = nil,
            Ny_2 = nil,
            Pct_4 = nil,
            Res_2 = nil,
            Transition_2 = nil,
            Trend_2 = nil;
        _ -> DaysToAdd = ((((Quadrant + 1) * Length) div 4) - Position),
            Res = adddays(Ty, Tm, Td, DaysToAdd),
            Ny = lists:nth(0 + 1, Res),
            Nm = lists:nth(1 + 1, Res),
            Nd = lists:nth(2 + 1, Res),
            Transition = datestring(Ny, Nm, Nd),
            Trend = lists:nth(0 + 1, lists:nth(Quadrant + 1, Quadrants)),
            Next = lists:nth(1 + 1, lists:nth(Quadrant + 1, Quadrants)),
            Pct = lists:flatten(io_lib:format("~p", [Percent_2])),
            case not (string:str(Pct, ".") =/= 0) of
        true -> Pct_2 = (Pct ++ ".0"),
            Pct_3 = Pct_2;
        _ -> Pct_3 = Pct
    end,
            Description_5 = ((((((((" " ++ Pct_3) ++ "% (") ++ Trend) ++ ", next ") ++ Next) ++ " ") ++ Transition) ++ ")"),
            DaysToAdd_2 = DaysToAdd,
            Description_6 = Description_5,
            Nd_2 = Nd,
            Next_2 = Next,
            Nm_2 = Nm,
            Ny_2 = Ny,
            Pct_4 = Pct_3,
            Res_2 = Res,
            Transition_2 = Transition,
            Trend_2 = Trend
    end,
            DaysToAdd_3 = DaysToAdd_2,
            Description_7 = Description_6,
            Nd_3 = Nd_2,
            Next_3 = Next_2,
            Nm_3 = Nm_2,
            Ny_3 = Ny_2,
            Pct_5 = Pct_4,
            Res_3 = Res_2,
            Transition_3 = Transition_2,
            Trend_3 = Trend_2
    end,
            DaysToAdd_4 = DaysToAdd_3,
            Description_8 = Description_7,
            Nd_4 = Nd_3,
            Next_4 = Next_3,
            Nm_4 = Nm_3,
            Ny_4 = Ny_3,
            Pct_6 = Pct_5,
            Res_4 = Res_3,
            Transition_4 = Transition_3,
            Trend_4 = Trend_3
    end,
            PosStr = lists:flatten(io_lib:format("~p", [Position])),
            case (Position < 10) of
        true -> PosStr_2 = (" " ++ PosStr),
            PosStr_3 = PosStr_2;
        _ -> PosStr_3 = PosStr
    end,
            io:format("~ts~n", [(((Cycle ++ PosStr_3) ++ " : ") ++ Description_8)]),
            I_14 = (I_13 + 1),
            Fun_6_loop(Bd, Birth, Bm, Bparts, By, Cycles, Diff, I_14, Lengths_2, Quadrants, Target, Td, Tm, Tparts, Ty);
        _ -> {Bd, Birth, Bm, Bparts, By, Cycles, Diff, I_13, Lengths_2, Quadrants, Target, Td, Tm, Tparts, Ty}
    end
end,
{Bd, Birth, Bm, Bparts, By, Cycles, Diff, I_14, Lengths_2, Quadrants, Target, Td, Tm, Tparts, Ty} = Fun_6(Bd, Birth, Bm, Bparts, By, Cycles, Diff, I_13, Lengths_2, Quadrants, Target, Td, Tm, Tparts, Ty),
        io:format("~ts~n", [""]),
        nil
    catch {return, Ret} -> Ret end.

main() ->
    try
        Pairs = [["1943-03-09", "1972-07-11"], ["1809-01-12", "1863-11-19"], ["1809-02-12", "1863-11-19"]],
        Idx = 0,
        Fun_7 = fun Fun_7_loop(Idx, Pairs) ->
    case (Idx < length(Pairs)) of
        true ->
            P = lists:nth(Idx + 1, Pairs),
            biorhythms(lists:nth(0 + 1, P), lists:nth(1 + 1, P)),
            Idx_2 = (Idx + 1),
            Fun_7_loop(Idx_2, Pairs);
        _ -> {Idx, Pairs}
    end
end,
{Idx_2, Pairs} = Fun_7(Idx, Pairs),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    Start = mochi_now(),
    StartMem = erlang:memory(total),
    erlang:put('PI', 3.141592653589793),
    erlang:put('TWO_PI', 6.283185307179586),
    main(),
    End = mochi_now(),
    EndMem = erlang:memory(total),
    DurationUs = (End - Start) div 1000,
    MemBytes = abs(EndMem - StartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
