#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, isleap/1, daysinmonth/2, daysbeforeyear/1, daysbeforemonth/2, epochseconds/5, fromepoch/1, pad2/1, absint/1, formatdate/3, parseintstr/1, indexof/2, parsetime/1, main/0]).

% Generated by Mochi transpiler v0.10.42 (fd9ce91b2e) on 2025-07-28 04:37 UTC


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


mochi_parse_int_str(S) ->
    try list_to_integer(S) catch _:_ -> 0 end.

isleap(Y) ->
    try
        case ((Y rem 400) == 0) of
        true -> throw({return, true});
        _ -> ok
    end,
        case ((Y rem 100) == 0) of
        true -> throw({return, false});
        _ -> ok
    end,
        ((Y rem 4) == 0)
    catch {return, Ret} -> Ret end.

daysinmonth(Y_2, M) ->
    try
        Feb = (case isleap(Y_2) of
    true -> 29;
    _ -> 28
end),
        Lengths = [31, Feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
        lists:nth((M - 1) + 1, Lengths)
    catch {return, Ret} -> Ret end.

daysbeforeyear(Y_3) ->
    try
        Days = 0,
        Yy = 1970,
        Fun = fun Fun_loop(Days, Y_3, Yy) ->
    case (Yy < Y_3) of
        true ->
            Days_2 = (Days + 365),
            case (isleap(Yy) /= nil) of
        true -> Days_3 = (Days_2 + 1),
            Days_4 = Days_3;
        _ -> Days_4 = Days_2
    end,
            Yy_2 = (Yy + 1),
            Fun_loop(Days_4, Y_3, Yy_2);
        _ -> {Days, Y_3, Yy}
    end
end,
{Days_4, Y_3, Yy_2} = Fun(Days, Y_3, Yy),
        Days_4
    catch {return, Ret} -> Ret end.

daysbeforemonth(Y_4, M_2) ->
    try
        Days_5 = 0,
        Mm = 1,
        Fun_2 = fun Fun_2_loop(Days_5, M_2, Mm, Y_4) ->
    case (Mm < M_2) of
        true ->
            Days_6 = (Days_5 + daysinmonth(Y_4, Mm)),
            Mm_2 = (Mm + 1),
            Fun_2_loop(Days_6, M_2, Mm_2, Y_4);
        _ -> {Days_5, M_2, Mm, Y_4}
    end
end,
{Days_6, M_2, Mm_2, Y_4} = Fun_2(Days_5, M_2, Mm, Y_4),
        Days_6
    catch {return, Ret} -> Ret end.

epochseconds(Y_5, M_3, D, H, Mi) ->
    try
        Days_7 = ((daysbeforeyear(Y_5) + daysbeforemonth(Y_5, M_3)) + (D - 1)),
        (((Days_7 * 86400) + (H * 3600)) + (Mi * 60))
    catch {return, Ret} -> Ret end.

fromepoch(Sec) ->
    try
        Days_8 = (Sec div 86400),
        Rem = (Sec rem 86400),
        Y_6 = 1970,
        Fun_3 = fun Fun_3_loop(Days_8, Rem, Sec, Y_6) ->
    case true of
        true ->
            try
                Dy = (case isleap(Y_6) of
    true -> 366;
    _ -> 365
end),
                case (Days_8 >= Dy) of
        true -> Days_9 = (Days_8 - Dy),
            Y_7 = (Y_6 + 1),
            Days_10 = Days_9,
            Y_8 = Y_7;
        _ -> throw(break),
            Days_10 = Days_8,
            Y_8 = Y_6
    end,
                Fun_3_loop(Days_10, Rem, Sec, Y_8)
            catch
                {continue, C0, C1, C2, C3} -> Fun_3_loop(C0, C1, C2, C3);
                break -> {Days_8, Rem, Sec, Y_6}
            end;
        _ -> {Days_8, Rem, Sec, Y_6}
    end
end,
{Days_10, Rem, Sec, Y_8} = Fun_3(Days_8, Rem, Sec, Y_6),
        M_4 = 1,
        Fun_4 = fun Fun_4_loop(Days_10, M_4, Rem, Sec, Y_8) ->
    case true of
        true ->
            try
                Dim = daysinmonth(Y_8, M_4),
                case (Days_10 >= Dim) of
        true -> Days_11 = (Days_10 - Dim),
            M_5 = (M_4 + 1),
            Days_12 = Days_11,
            M_6 = M_5;
        _ -> throw(break),
            Days_12 = Days_10,
            M_6 = M_4
    end,
                Fun_4_loop(Days_12, M_6, Rem, Sec, Y_8)
            catch
                {continue, C0, C1, C2, C3, C4} -> Fun_4_loop(C0, C1, C2, C3, C4);
                break -> {Days_10, M_4, Rem, Sec, Y_8}
            end;
        _ -> {Days_10, M_4, Rem, Sec, Y_8}
    end
end,
{Days_12, M_6, Rem, Sec, Y_8} = Fun_4(Days_10, M_4, Rem, Sec, Y_8),
        D_2 = (Days_12 + 1),
        H_2 = (Rem div 3600),
        Mi_2 = ((Rem rem 3600) div 60),
        [Y_8, M_6, D_2, H_2, Mi_2]
    catch {return, Ret} -> Ret end.

pad2(N) ->
    try
        (case (N < 10) of
    true -> ("0" ++ lists:flatten(io_lib:format("~p", [N])));
    _ -> lists:flatten(io_lib:format("~p", [N]))
end)
    catch {return, Ret} -> Ret end.

absint(N_2) ->
    try
        (case (N_2 < 0) of
    true -> -N_2;
    _ -> N_2
end)
    catch {return, Ret} -> Ret end.

formatdate(Parts, Offset, Abbr) ->
    try
        Y_9 = lists:nth(0 + 1, Parts),
        M_7 = lists:nth(1 + 1, Parts),
        D_3 = lists:nth(2 + 1, Parts),
        H_3 = lists:nth(3 + 1, Parts),
        Mi_3 = lists:nth(4 + 1, Parts),
        Sign = "+",
        case (Offset < 0) of
        true -> Sign_2 = "-",
            Sign_3 = Sign_2;
        _ -> Sign_3 = Sign
    end,
        Off = (absint(Offset) / 60),
        Offh = pad2((Off / 60)),
        Offm = pad2(math:fmod(Off, 60)),
        ((((((((((((((lists:flatten(io_lib:format("~p", [Y_9])) ++ "-") ++ pad2(M_7)) ++ "-") ++ pad2(D_3)) ++ " ") ++ pad2(H_3)) ++ ":") ++ pad2(Mi_3)) ++ ":00 ") ++ Sign_3) ++ Offh) ++ Offm) ++ " ") ++ Abbr)
    catch {return, Ret} -> Ret end.

parseintstr(Str) ->
    try
        I = 0,
        Neg = false,
        case ((length(Str) > 0) andalso (string:substr(Str, 0 + 1, (1 - 0)) == "-")) of
        true -> Neg_2 = true,
            I_2 = 1,
            I_3 = I_2,
            Neg_3 = Neg_2;
        _ -> I_3 = I,
            Neg_3 = Neg
    end,
        N_3 = 0,
        Digits = #{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9},
        Fun_5 = fun Fun_5_loop(Digits, I_3, N_3, Neg_3, Str) ->
    case (I_3 < length(Str)) of
        true ->
            N_4 = ((N_3 * 10) + maps:get(string:substr(Str, I_3 + 1, ((I_3 + 1) - I_3)), Digits, nil)),
            I_4 = (I_3 + 1),
            Fun_5_loop(Digits, I_4, N_4, Neg_3, Str);
        _ -> {Digits, I_3, N_3, Neg_3, Str}
    end
end,
{Digits, I_4, N_4, Neg_3, Str} = Fun_5(Digits, I_3, N_3, Neg_3, Str),
        case Neg_3 of
        true -> N_5 = -N_4,
            N_6 = N_5;
        _ -> N_6 = N_4
    end,
        N_6
    catch {return, Ret} -> Ret end.

indexof(S, Ch) ->
    try
        I_5 = 0,
        Fun_6 = fun Fun_6_loop(Ch, I_5, S) ->
    case (I_5 < length(S)) of
        true ->
            case (string:substr(S, I_5 + 1, ((I_5 + 1) - I_5)) == Ch) of
        true -> throw({return, I_5});
        _ -> ok
    end,
            I_6 = (I_5 + 1),
            Fun_6_loop(Ch, I_6, S);
        _ -> {Ch, I_5, S}
    end
end,
{Ch, I_6, S} = Fun_6(Ch, I_5, S),
        -1
    catch {return, Ret} -> Ret end.

parsetime(S_2) ->
    try
        C = mochi_index_of(S_2, ":"),
        H_4 = mochi_parse_int_str(string:substr(S_2, 0 + 1, (C - 0))),
        Mi_4 = mochi_parse_int_str(string:substr(S_2, (C + 1) + 1, ((C + 3) - (C + 1)))),
        Ampm = string:substr(S_2, (length(S_2) - 2) + 1, (length(S_2) - (length(S_2) - 2))),
        Hh = H_4,
        case ((Ampm == "pm") andalso (H_4 /= 12)) of
        true -> Hh_2 = (H_4 + 12),
            Hh_3 = Hh_2;
        _ -> Hh_3 = Hh
    end,
        case ((Ampm == "am") andalso (H_4 == 12)) of
        true -> Hh_4 = 0,
            Hh_5 = Hh_4;
        _ -> Hh_5 = Hh_3
    end,
        [Hh_5, Mi_4]
    catch {return, Ret} -> Ret end.

main() ->
    try
        Input = "March 7 2009 7:30pm EST",
        io:format("~ts~n", [("Input:              " ++ "March 7 2009 7:30pm EST")]),
        Parts_2 = [],
        Cur = "",
        I_7 = 0,
        Fun_7 = fun Fun_7_loop(Cur, I_7, Input, Parts_2) ->
    case (I_7 < length(Input)) of
        true ->
            Ch_2 = string:substr(Input, I_7 + 1, ((I_7 + 1) - I_7)),
            case (Ch_2 == " ") of
        true -> case (length(Cur) > 0) of
        true -> Parts_3 = lists:append(Parts_2, [Cur]),
            Cur_2 = "",
            Cur_3 = Cur_2,
            Parts_4 = Parts_3;
        _ -> Cur_3 = Cur,
            Parts_4 = Parts_2
    end,
            Cur_5 = Cur_3,
            Parts_5 = Parts_4;
        _ -> Cur_4 = (Cur ++ Ch_2),
            Cur_5 = Cur_4,
            Parts_5 = Parts_2
    end,
            I_8 = (I_7 + 1),
            Fun_7_loop(Cur_5, I_8, Input, Parts_5);
        _ -> {Cur, I_7, Input, Parts_2}
    end
end,
{Cur_5, I_8, Input, Parts_5} = Fun_7(Cur, I_7, Input, Parts_2),
        case (length(Cur_5) > 0) of
        true -> Parts_6 = lists:append(Parts_5, [Cur_5]),
            Parts_7 = Parts_6;
        _ -> Parts_7 = Parts_5
    end,
        Month = maps:get(lists:nth(0 + 1, Parts_7), erlang:get('months'), nil),
        Day = mochi_parse_int_str(lists:nth(1 + 1, Parts_7)),
        Year = mochi_parse_int_str(lists:nth(2 + 1, Parts_7)),
        Tm = parsetime(lists:nth(3 + 1, Parts_7)),
        Hour = lists:nth(0 + 1, Tm),
        Minute = lists:nth(1 + 1, Tm),
        Tz = lists:nth(4 + 1, Parts_7),
        ZoneOffsets = #{"EST" => -18000, "EDT" => -14400, "MST" => -25200},
        Local = epochseconds(Year, Month, Day, Hour, Minute),
        Utc = (Local - maps:get(Tz, ZoneOffsets, nil)),
        Utc12 = (Utc + 43200),
        StartDST = epochseconds(2009, 3, 8, 7, 0),
        OffEast = -18000,
        case (Utc12 >= StartDST) of
        true -> OffEast_2 = -14400,
            OffEast_3 = OffEast_2;
        _ -> OffEast_3 = OffEast
    end,
        EastParts = fromepoch((Utc12 + OffEast_3)),
        EastAbbr = "EST",
        case (OffEast_3 == -14400) of
        true -> EastAbbr_2 = "EDT",
            EastAbbr_3 = EastAbbr_2;
        _ -> EastAbbr_3 = EastAbbr
    end,
        io:format("~ts~n", [("+12 hrs:            " ++ formatdate(EastParts, OffEast_3, EastAbbr_3))]),
        OffAZ = -25200,
        AzParts = fromepoch((Utc12 + OffAZ)),
        io:format("~ts~n", [("+12 hrs in Arizona: " ++ formatdate(AzParts, OffAZ, "MST"))]),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('months', #{"January" => 1, "February" => 2, "March" => 3, "April" => 4, "May" => 5, "June" => 6, "July" => 7, "August" => 8, "September" => 9, "October" => 10, "November" => 11, "December" => 12}),
    main(),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
