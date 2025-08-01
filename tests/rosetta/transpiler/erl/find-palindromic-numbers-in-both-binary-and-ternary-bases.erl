#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, tobase/2, parseintbase/2, reversestr/1, ispalindrome/1, ispalindromebin/1, mymin/2, mymax/2, reverse3/1, show/1, main/0]).

% Generated by Mochi transpiler v0.10.50 (e649287d54) on 2025-07-30 14:16 UTC


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


mochi_parse_int_str(S) ->
    try list_to_integer(S) catch _:_ -> 0 end.

tobase(N, B) ->
    try
        case (N == 0) of
        true -> throw({return, "0"});
        _ -> ok
    end,
        S = "",
        X = N,
        Fun = fun Fun_loop(B, N, S, X) ->
    case (X > 0) of
        true ->
            S_2 = (lists:flatten(io_lib:format("~p", [(X rem B)])) ++ S),
            X_2 = mochi_to_int((X / B)),
            Fun_loop(B, N, S_2, X_2);
        _ -> {B, N, S, X}
    end
end,
{B, N, S_2, X_2} = Fun(B, N, S, X),
        throw({return, S_2}),
        ParseIntStr = fun ParseIntStr(Str) ->
    try
        I = 0,
        Neg = false,
        case ((length(Str) > 0) andalso (lists:nth(0 + 1, Str) == "-")) of
        true -> Neg_2 = true,
            I_2 = 1,
            I_3 = I_2,
            Neg_3 = Neg_2;
        _ -> I_3 = I,
            Neg_3 = Neg
    end,
        N_2 = 0,
        Fun_2 = fun Fun_2_loop(B, I_3, N_2, Neg_3, ParseIntStr, S_2, Str, X_2) ->
    case (I_3 < length(Str)) of
        true ->
            N_3 = (((N_2 * 10) + mochi_to_int(lists:sublist(Str, I_3 + 1, ((I_3 + 1) - I_3)))) - mochi_to_int("0")),
            I_4 = (I_3 + 1),
            Fun_2_loop(B, I_4, N_3, Neg_3, ParseIntStr, S_2, Str, X_2);
        _ -> {B, I_3, N_2, Neg_3, ParseIntStr, S_2, Str, X_2}
    end
end,
{B, I_4, N_3, Neg_3, ParseIntStr, S_2, Str, X_2} = Fun_2(B, I_3, N_2, Neg_3, ParseIntStr, S_2, Str, X_2),
        case Neg_3 of
        true -> N_4 = -N_3,
            N_5 = N_4;
        _ -> N_5 = N_3
    end,
        N_5
    catch {return, Ret} -> Ret end
end,
        nil
    catch {return, Ret} -> Ret end.

parseintbase(S_3, B_2) ->
    try
        N_6 = 0,
        I_5 = 0,
        Fun_3 = fun Fun_3_loop(B_2, I_5, N_6, S_3) ->
    case (I_5 < length(S_3)) of
        true ->
            N_7 = ((N_6 * B_2) + mochi_parse_int_str(string:substr(S_3, I_5 + 1, ((I_5 + 1) - I_5)))),
            I_6 = (I_5 + 1),
            Fun_3_loop(B_2, I_6, N_7, S_3);
        _ -> {B_2, I_5, N_6, S_3}
    end
end,
{B_2, I_6, N_7, S_3} = Fun_3(B_2, I_5, N_6, S_3),
        N_7
    catch {return, Ret} -> Ret end.

reversestr(S_4) ->
    try
        Out = "",
        I_7 = (length(S_4) - 1),
        Fun_4 = fun Fun_4_loop(I_7, Out, S_4) ->
    case (I_7 >= 0) of
        true ->
            Out_2 = (Out ++ string:substr(S_4, I_7 + 1, ((I_7 + 1) - I_7))),
            I_8 = (I_7 - 1),
            Fun_4_loop(I_8, Out_2, S_4);
        _ -> {I_7, Out, S_4}
    end
end,
{I_8, Out_2, S_4} = Fun_4(I_7, Out, S_4),
        Out_2
    catch {return, Ret} -> Ret end.

ispalindrome(S_5) ->
    try
        (S_5 == reversestr(S_5))
    catch {return, Ret} -> Ret end.

ispalindromebin(N_8) ->
    try
        B_3 = tobase(N_8, 2),
        ispalindrome(B_3)
    catch {return, Ret} -> Ret end.

mymin(A, B_4) ->
    try
        (case (A < B_4) of
    true -> A;
    _ -> B_4
end)
    catch {return, Ret} -> Ret end.

mymax(A_2, B_5) ->
    try
        (case (A_2 > B_5) of
    true -> A_2;
    _ -> B_5
end)
    catch {return, Ret} -> Ret end.

reverse3(N_9) ->
    try
        X_3 = 0,
        Y = N_9,
        Fun_5 = fun Fun_5_loop(N_9, X_3, Y) ->
    case (Y /= 0) of
        true ->
            X_4 = ((X_3 * 3) + (Y rem 3)),
            Y_2 = mochi_to_int((Y / 3)),
            Fun_5_loop(N_9, X_4, Y_2);
        _ -> {N_9, X_3, Y}
    end
end,
{N_9, X_4, Y_2} = Fun_5(N_9, X_3, Y),
        X_4
    catch {return, Ret} -> Ret end.

show(N_10) ->
    try
        io:format("~ts~n", [("Decimal : " ++ lists:flatten(io_lib:format("~p", [N_10])))]),
        io:format("~ts~n", [("Binary  : " ++ tobase(N_10, 2))]),
        io:format("~ts~n", [("Ternary : " ++ tobase(N_10, 3))]),
        io:format("~ts~n", [""]),
        nil
    catch {return, Ret} -> Ret end.

main() ->
    try
        io:format("~ts~n", ["The first 6 numbers which are palindromic in both binary and ternary are :\n"]),
        show(0),
        Count = 1,
        Lo = 0,
        Hi = 1,
        Pow2 = 1,
        Pow3 = 1,
        Fun_9 = fun Fun_9_loop(Count, Hi, Lo, Pow2, Pow3) ->
    case true of
        true ->
            try
                I_9 = Lo,
                Fun_6 = fun Fun_6_loop(Count, Hi, I_9, Lo, Pow2, Pow3) ->
    case (I_9 < Hi) of
        true ->
            N_11 = ((((I_9 * 3) + 1) * Pow3) + reverse3(I_9)),
            case (ispalindromebin(N_11) /= nil) of
        true -> show(N_11),
            Count_2 = (Count + 1),
            case (Count_2 >= 6) of
        true -> throw({return, nil});
        _ -> ok
    end,
            Count_3 = Count_2;
        _ -> Count_3 = Count
    end,
            I_10 = (I_9 + 1),
            Fun_6_loop(Count_3, Hi, I_10, Lo, Pow2, Pow3);
        _ -> {Count, Hi, I_9, Lo, Pow2, Pow3}
    end
end,
{Count_3, Hi, I_10, Lo, Pow2, Pow3} = Fun_6(Count, Hi, I_9, Lo, Pow2, Pow3),
                case (I_10 == Pow3) of
        true -> Pow3_2 = (Pow3 * 3),
            Pow2_3 = Pow2,
            Pow3_3 = Pow3_2;
        _ -> Pow2_2 = (Pow2 * 4),
            Pow2_3 = Pow2_2,
            Pow3_3 = Pow3
    end,
                Fun_8 = fun Fun_8_loop(Count_3, Hi, I_10, Lo, Pow2_3, Pow3_3) ->
    case true of
        true ->
            try
                Fun_7 = fun Fun_7_loop(Count_3, Hi, I_10, Lo, Pow2_3, Pow3_3) ->
    case (Pow2_3 =< Pow3_3) of
        true ->
            Pow2_4 = (Pow2_3 * 4),
            Fun_7_loop(Count_3, Hi, I_10, Lo, Pow2_4, Pow3_3);
        _ -> {Count_3, Hi, I_10, Lo, Pow2_3, Pow3_3}
    end
end,
{Count_3, Hi, I_10, Lo, Pow2_4, Pow3_3} = Fun_7(Count_3, Hi, I_10, Lo, Pow2_3, Pow3_3),
                Lo2 = mochi_to_int((((Pow2_4 / Pow3_3) - 1) / 3)),
                Hi2 = (mochi_to_int(((((Pow2_4 * 2) / Pow3_3) - 1) / 3)) + 1),
                Lo3 = mochi_to_int((Pow3_3 / 3)),
                Hi3 = Pow3_3,
                case (Lo2 >= Hi3) of
        true -> Pow3_4 = (Pow3_3 * 3),
            Hi_4 = Hi,
            Lo_4 = Lo,
            Pow2_7 = Pow2_4,
            Pow3_5 = Pow3_4;
        _ -> case (Lo3 >= Hi2) of
        true -> Pow2_5 = (Pow2_4 * 4),
            Hi_3 = Hi,
            Lo_3 = Lo,
            Pow2_6 = Pow2_5;
        _ -> Lo_2 = mymax(Lo2, Lo3),
            Hi_2 = mymin(Hi2, Hi3),
            throw(break),
            Hi_3 = Hi_2,
            Lo_3 = Lo_2,
            Pow2_6 = Pow2_4
    end,
            Hi_4 = Hi_3,
            Lo_4 = Lo_3,
            Pow2_7 = Pow2_6,
            Pow3_5 = Pow3_3
    end,
                Fun_8_loop(Count_3, Hi_4, I_10, Lo_4, Pow2_7, Pow3_5)
            catch
                {continue, C0, C1, C2, C3, C4, C5} -> Fun_8_loop(C0, C1, C2, C3, C4, C5);
                break -> {Count_3, Hi, I_10, Lo, Pow2_3, Pow3_3}
            end;
        _ -> {Count_3, Hi, I_10, Lo, Pow2_3, Pow3_3}
    end
end,
{Count_3, Hi_4, I_10, Lo_4, Pow2_7, Pow3_5} = Fun_8(Count_3, Hi, I_10, Lo, Pow2_3, Pow3_3),
                Fun_9_loop(Count_3, Hi_4, Lo_4, Pow2_7, Pow3_5)
            catch
                {continue, C0, C1, C2, C3, C4} -> Fun_9_loop(C0, C1, C2, C3, C4);
                break -> {Count, Hi, Lo, Pow2, Pow3}
            end;
        _ -> {Count, Hi, Lo, Pow2, Pow3}
    end
end,
{Count_3, Hi_4, Lo_4, Pow2_7, Pow3_5} = Fun_9(Count, Hi, Lo, Pow2, Pow3),
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
