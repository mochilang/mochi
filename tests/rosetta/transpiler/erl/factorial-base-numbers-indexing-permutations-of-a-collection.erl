#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, split/2, parseintstr/1, joinints/2, undot/1, factorial/1, genfactbasenums/2, maptoperms/1, randint/1, main/0]).

% Generated by Mochi transpiler v0.10.55 (6aa59f472e) on 2025-08-02 17:26 +0700


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


mochi_parse_int_str(S) ->
    try list_to_integer(S) catch _:_ -> 0 end.


mochi_not(X) ->
    case X of
        true -> false;
        false -> true;
        nil -> true;
        _ -> false
    end.

split(S, Sep) ->
    try
        Parts = [],
        Cur = "",
        I = 0,
        Fun = fun Fun_loop(Cur, I, Parts, S, Sep) ->
    case (I < length(S)) of
        true ->
            case (((length(Sep) > 0) andalso ((I + length(Sep)) =< length(S))) andalso (string:substr(S, I + 1, ((I + length(Sep)) - I)) == Sep)) of
        true -> Parts_2 = lists:append(Parts, [Cur]),
            Cur_2 = "",
            I_2 = (I + length(Sep)),
            Cur_4 = Cur_2,
            I_4 = I_2,
            Parts_3 = Parts_2;
        _ -> Cur_3 = (Cur ++ string:substr(S, I + 1, ((I + 1) - I))),
            I_3 = (I + 1),
            Cur_4 = Cur_3,
            I_4 = I_3,
            Parts_3 = Parts
    end,
            Fun_loop(Cur_4, I_4, Parts_3, S, Sep);
        _ -> {Cur, I, Parts, S, Sep}
    end
end,
{Cur_4, I_4, Parts_3, S, Sep} = Fun(Cur, I, Parts, S, Sep),
        Parts_4 = lists:append(Parts_3, [Cur_4]),
        Parts_4
    catch {return, Ret} -> Ret end.

parseintstr(Str) ->
    try
        I_5 = 0,
        Neg = false,
        case ((length(Str) > 0) andalso (string:substr(Str, 0 + 1, (1 - 0)) == "-")) of
        true -> Neg_2 = true,
            I_6 = 1,
            I_7 = I_6,
            Neg_3 = Neg_2;
        _ -> I_7 = I_5,
            Neg_3 = Neg
    end,
        N = 0,
        Digits = #{"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9},
        Fun_2 = fun Fun_2_loop(Digits, I_7, N, Neg_3, Str) ->
    case (I_7 < length(Str)) of
        true ->
            N_2 = ((N * 10) + maps:get(string:substr(Str, I_7 + 1, ((I_7 + 1) - I_7)), Digits, nil)),
            I_8 = (I_7 + 1),
            Fun_2_loop(Digits, I_8, N_2, Neg_3, Str);
        _ -> {Digits, I_7, N, Neg_3, Str}
    end
end,
{Digits, I_8, N_2, Neg_3, Str} = Fun_2(Digits, I_7, N, Neg_3, Str),
        case Neg_3 of
        true -> N_3 = -N_2,
            N_4 = N_3;
        _ -> N_4 = N_2
    end,
        N_4
    catch {return, Ret} -> Ret end.

joinints(Nums, Sep_2) ->
    try
        S_2 = "",
        I_9 = 0,
        Fun_3 = fun Fun_3_loop(I_9, Nums, S_2, Sep_2) ->
    case (I_9 < length(Nums)) of
        true ->
            case (I_9 > 0) of
        true -> S_3 = (S_2 ++ Sep_2),
            S_4 = S_3;
        _ -> S_4 = S_2
    end,
            S_5 = (S_4 ++ lists:flatten(io_lib:format("~p", [(case erlang:is_map(Nums) of true -> maps:get(I_9, Nums, nil); _ -> lists:nth(I_9 + 1, Nums) end)]))),
            I_10 = (I_9 + 1),
            Fun_3_loop(I_10, Nums, S_5, Sep_2);
        _ -> {I_9, Nums, S_2, Sep_2}
    end
end,
{I_10, Nums, S_5, Sep_2} = Fun_3(I_9, Nums, S_2, Sep_2),
        S_5
    catch {return, Ret} -> Ret end.

undot(S_6) ->
    try
        Parts_5 = string:tokens(S_6, "."),
        Nums_2 = [],
        Fun_4 = fun Fun_4_loop(List, Nums_2, Parts_5, S_6) ->
    case List of
        [] -> {Nums_2, Parts_5, S_6};
        [P|P_rest] ->
            Nums_3 = lists:append(Nums_2, [mochi_parse_int_str(P)]),
            Fun_4_loop(P_rest, Nums_3, Parts_5, S_6)
    end
end,
{Nums_3, Parts_5, S_6} = Fun_4(Parts_5, Nums_2, Parts_5, S_6),
        Nums_3
    catch {return, Ret} -> Ret end.

factorial(N_5) ->
    try
        F = 1,
        I_11 = 2,
        Fun_5 = fun Fun_5_loop(F, I_11, N_5) ->
    case (I_11 =< N_5) of
        true ->
            F_2 = (F * I_11),
            I_12 = (I_11 + 1),
            Fun_5_loop(F_2, I_12, N_5);
        _ -> {F, I_11, N_5}
    end
end,
{F_2, I_12, N_5} = Fun_5(F, I_11, N_5),
        F_2
    catch {return, Ret} -> Ret end.

genfactbasenums(Size, CountOnly) ->
    try
        Results = [],
        Count = 0,
        N_6 = 0,
        Fun_9 = fun Fun_9_loop(Count, CountOnly, N_6, Results, Size) ->
    case true of
        true ->
            try
                Radix = 2,
                Res = [],
                case mochi_not(CountOnly) of
        true -> Z = 0,
            Fun_6 = fun Fun_6_loop(Count, CountOnly, N_6, Radix, Res, Results, Size, Z) ->
    case (Z < Size) of
        true ->
            Res_2 = lists:append(Res, [0]),
            Z_2 = (Z + 1),
            Fun_6_loop(Count, CountOnly, N_6, Radix, Res_2, Results, Size, Z_2);
        _ -> {Count, CountOnly, N_6, Radix, Res, Results, Size, Z}
    end
end,
{Count, CountOnly, N_6, Radix, Res_2, Results, Size, Z_2} = Fun_6(Count, CountOnly, N_6, Radix, Res, Results, Size, Z),
            Fun_7 = Fun_6,
            Res_3 = Res_2,
            Z_3 = Z_2;
        _ -> Fun_7 = nil,
            Res_3 = Res,
            Z_3 = nil
    end,
                K = N_6,
                Fun_8 = fun Fun_8_loop(Count, CountOnly, K, N_6, Radix, Res_3, Results, Size, Z_3) ->
    case (K > 0) of
        true ->
            Div = (K div Radix),
            Rem = (K rem Radix),
            case (mochi_not(CountOnly) andalso (Radix =< (Size + 1))) of
        true -> Res_4 = lists:sublist(Res_3, ((Size - Radix) + 1)) ++ [Rem] ++ lists:nthtail(((Size - Radix) + 1) + 1, Res_3),
            Res_5 = Res_4;
        _ -> Res_5 = Res_3
    end,
            K_2 = Div,
            Radix_2 = (Radix + 1),
            Fun_8_loop(Count, CountOnly, K_2, N_6, Radix_2, Res_5, Results, Size, Z_3);
        _ -> {Count, CountOnly, K, N_6, Radix, Res_3, Results, Size, Z_3}
    end
end,
{Count, CountOnly, K_2, N_6, Radix_2, Res_5, Results, Size, Z_3} = Fun_8(Count, CountOnly, K, N_6, Radix, Res_3, Results, Size, Z_3),
                case (Radix_2 > (Size + 2)) of
        true -> throw(break);
        _ -> ok
    end,
                Count_2 = (Count + 1),
                case mochi_not(CountOnly) of
        true -> Results_2 = lists:append(Results, [Res_5]),
            Results_3 = Results_2;
        _ -> Results_3 = Results
    end,
                N_7 = (N_6 + 1),
                Fun_9_loop(Count_2, CountOnly, N_7, Results_3, Size)
            catch
                {continue, C0, C1, C2, C3, C4} -> Fun_9_loop(C0, C1, C2, C3, C4);
                break -> {Count, CountOnly, N_6, Results, Size}
            end;
        _ -> {Count, CountOnly, N_6, Results, Size}
    end
end,
{Count_2, CountOnly, N_7, Results_3, Size} = Fun_9(Count, CountOnly, N_6, Results, Size),
        [Results_3, Count_2]
    catch {return, Ret} -> Ret end.

maptoperms(FactNums) ->
    try
        Perms = [],
        Psize = (length((case erlang:is_map(FactNums) of true -> maps:get(0, FactNums, nil); _ -> lists:nth(0 + 1, FactNums) end)) + 1),
        Start = [],
        I_13 = 0,
        Fun_10 = fun Fun_10_loop(FactNums, I_13, Perms, Psize, Start) ->
    case (I_13 < Psize) of
        true ->
            Start_2 = lists:append(Start, [I_13]),
            I_14 = (I_13 + 1),
            Fun_10_loop(FactNums, I_14, Perms, Psize, Start_2);
        _ -> {FactNums, I_13, Perms, Psize, Start}
    end
end,
{FactNums, I_14, Perms, Psize, Start_2} = Fun_10(FactNums, I_13, Perms, Psize, Start),
        Fun_11 = fun Fun_11_loop(List, FactNums, I_14, Perms, Psize, Start_2) ->
    case List of
        [] -> {FactNums, I_14, Perms, Psize, Start_2};
        [Fn|Fn_rest] ->
            Perm = [],
            J = 0,
            Fun_12 = fun Fun_12_loop(FactNums, Fn, I_14, J, Perm, Perms, Psize, Start_2) ->
    case (J < length(Start_2)) of
        true ->
            Perm_2 = lists:append(Perm, [(case erlang:is_map(Start_2) of true -> maps:get(J, Start_2, nil); _ -> lists:nth(J + 1, Start_2) end)]),
            J_2 = (J + 1),
            Fun_12_loop(FactNums, Fn, I_14, J_2, Perm_2, Perms, Psize, Start_2);
        _ -> {FactNums, Fn, I_14, J, Perm, Perms, Psize, Start_2}
    end
end,
{FactNums, Fn, I_14, J_2, Perm_2, Perms, Psize, Start_2} = Fun_12(FactNums, Fn, I_14, J, Perm, Perms, Psize, Start_2),
            M = 0,
            Fun_16 = fun Fun_16_loop(FactNums, Fn, I_14, J_2, M, Perm_2, Perms, Psize, Start_2) ->
    case (M < length(Fn)) of
        true ->
            G = (case erlang:is_map(Fn) of true -> maps:get(M, Fn, nil); _ -> lists:nth(M + 1, Fn) end),
            case (G /= 0) of
        true -> First = M,
            Last = (M + G),
            T = 1,
            Fun_14 = fun Fun_14_loop(FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_2, Perms, Psize, Start_2, T) ->
    case (T =< G) of
        true ->
            Temp = (case erlang:is_map(Perm_2) of true -> maps:get(First, Perm_2, nil); _ -> lists:nth(First + 1, Perm_2) end),
            X = (First + 1),
            Fun_13 = fun Fun_13_loop(FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_2, Perms, Psize, Start_2, T, Temp, X) ->
    case (X =< Last) of
        true ->
            Perm_3 = lists:sublist(Perm_2, (X - 1)) ++ [(case erlang:is_map(Perm_2) of true -> maps:get(X, Perm_2, nil); _ -> lists:nth(X + 1, Perm_2) end)] ++ lists:nthtail((X - 1) + 1, Perm_2),
            X_2 = (X + 1),
            Fun_13_loop(FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_3, Perms, Psize, Start_2, T, Temp, X_2);
        _ -> {FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_2, Perms, Psize, Start_2, T, Temp, X}
    end
end,
{FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_3, Perms, Psize, Start_2, T, Temp, X_2} = Fun_13(FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_2, Perms, Psize, Start_2, T, Temp, X),
            Perm_4 = lists:sublist(Perm_3, Last) ++ [Temp] ++ lists:nthtail(Last + 1, Perm_3),
            T_2 = (T + 1),
            Fun_14_loop(FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_4, Perms, Psize, Start_2, T_2);
        _ -> {FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_2, Perms, Psize, Start_2, T}
    end
end,
{FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_4, Perms, Psize, Start_2, T_2} = Fun_14(FactNums, First, Fn, G, I_14, J_2, Last, M, Perm_2, Perms, Psize, Start_2, T),
            First_2 = First,
            Fun_15 = Fun_14,
            Last_2 = Last,
            Perm_5 = Perm_4,
            T_3 = T_2;
        _ -> First_2 = nil,
            Fun_15 = Fun_12,
            Last_2 = nil,
            Perm_5 = Perm_2,
            T_3 = nil
    end,
            M_2 = (M + 1),
            Fun_16_loop(FactNums, Fn, I_14, J_2, M_2, Perm_5, Perms, Psize, Start_2);
        _ -> {FactNums, Fn, I_14, J_2, M, Perm_2, Perms, Psize, Start_2}
    end
end,
{FactNums, Fn, I_14, J_2, M_2, Perm_5, Perms, Psize, Start_2} = Fun_16(FactNums, Fn, I_14, J_2, M, Perm_2, Perms, Psize, Start_2),
            Perms_2 = lists:append(Perms, [Perm_5]),
            Fun_11_loop(Fn_rest, FactNums, I_14, Perms_2, Psize, Start_2)
    end
end,
{FactNums, I_14, Perms_2, Psize, Start_2} = Fun_11(FactNums, FactNums, I_14, Perms, Psize, Start_2),
        Perms_2
    catch {return, Ret} -> Ret end.

randint(N_8) ->
    try
        erlang:put('seed', (((erlang:get('seed') * 1664525) + 1013904223) rem 2147483647)),
        (erlang:get('seed') rem N_8)
    catch {return, Ret} -> Ret end.

main() ->
    try
        G_2 = genfactbasenums(3, false),
        FactNums_2 = (case erlang:is_map(G_2) of true -> maps:get(0, G_2, nil); _ -> lists:nth(0 + 1, G_2) end),
        Perms_3 = maptoperms(FactNums_2),
        I_15 = 0,
        Fun_17 = fun Fun_17_loop(FactNums_2, G_2, I_15, Perms_3) ->
    case (I_15 < length(FactNums_2)) of
        true ->
            io:format("~ts~n", [((joinints((case erlang:is_map(FactNums_2) of true -> maps:get(I_15, FactNums_2, nil); _ -> lists:nth(I_15 + 1, FactNums_2) end), ".") ++ " -> ") ++ joinints((case erlang:is_map(Perms_3) of true -> maps:get(I_15, Perms_3, nil); _ -> lists:nth(I_15 + 1, Perms_3) end), ""))]),
            I_16 = (I_15 + 1),
            Fun_17_loop(FactNums_2, G_2, I_16, Perms_3);
        _ -> {FactNums_2, G_2, I_15, Perms_3}
    end
end,
{FactNums_2, G_2, I_16, Perms_3} = Fun_17(FactNums_2, G_2, I_15, Perms_3),
        Count2 = factorial(11),
        io:format("~ts~n", [("\nPermutations generated = " ++ lists:flatten(io_lib:format("~p", [Count2])))]),
        io:format("~ts~n", [("compared to 11! which  = " ++ lists:flatten(io_lib:format("~p", [factorial(11)])))]),
        io:format("~ts~n", [""]),
        Fbn51s = ["39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0", "51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1"],
        FactNums_3 = [undot((case erlang:is_map(Fbn51s) of true -> maps:get(0, Fbn51s, nil); _ -> lists:nth(0 + 1, Fbn51s) end)), undot((case erlang:is_map(Fbn51s) of true -> maps:get(1, Fbn51s, nil); _ -> lists:nth(1 + 1, Fbn51s) end))],
        Perms_4 = maptoperms(FactNums_3),
        Shoe = "A♠K♠Q♠J♠T♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥T♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦T♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣T♣9♣8♣7♣6♣5♣4♣3♣2♣",
        Cards = [],
        I_17 = 0,
        Fun_18 = fun Fun_18_loop(Cards, Count2, FactNums_3, Fbn51s, G_2, I_17, Perms_4, Shoe) ->
    case (I_17 < 52) of
        true ->
            Card = string:substr(Shoe, (2 * I_17) + 1, (((2 * I_17) + 2) - (2 * I_17))),
            case (string:substr(Card, 0 + 1, (1 - 0)) == "T") of
        true -> Card_2 = ("10" ++ string:substr(Card, 1 + 1, (2 - 1))),
            Card_3 = Card_2;
        _ -> Card_3 = Card
    end,
            Cards_2 = lists:append(Cards, [Card_3]),
            I_18 = (I_17 + 1),
            Fun_18_loop(Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_18, Perms_4, Shoe);
        _ -> {Cards, Count2, FactNums_3, Fbn51s, G_2, I_17, Perms_4, Shoe}
    end
end,
{Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_18, Perms_4, Shoe} = Fun_18(Cards, Count2, FactNums_3, Fbn51s, G_2, I_17, Perms_4, Shoe),
        I_19 = 0,
        Fun_20 = fun Fun_20_loop(Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_19, Perms_4, Shoe) ->
    case (I_19 < length(Fbn51s)) of
        true ->
            io:format("~ts~n", [(case erlang:is_map(Fbn51s) of true -> maps:get(I_19, Fbn51s, nil); _ -> lists:nth(I_19 + 1, Fbn51s) end)]),
            Perm_6 = (case erlang:is_map(Perms_4) of true -> maps:get(I_19, Perms_4, nil); _ -> lists:nth(I_19 + 1, Perms_4) end),
            J_3 = 0,
            Line = "",
            Fun_19 = fun Fun_19_loop(Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_19, J_3, Line, Perm_6, Perms_4, Shoe) ->
    case (J_3 < length(Perm_6)) of
        true ->
            Line_2 = (Line ++ (case erlang:is_map(Cards_2) of true -> maps:get((case erlang:is_map(Perm_6) of true -> maps:get(J_3, Perm_6, nil); _ -> lists:nth(J_3 + 1, Perm_6) end), Cards_2, nil); _ -> lists:nth((case erlang:is_map(Perm_6) of true -> maps:get(J_3, Perm_6, nil); _ -> lists:nth(J_3 + 1, Perm_6) end) + 1, Cards_2) end)),
            J_4 = (J_3 + 1),
            Fun_19_loop(Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_19, J_4, Line_2, Perm_6, Perms_4, Shoe);
        _ -> {Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_19, J_3, Line, Perm_6, Perms_4, Shoe}
    end
end,
{Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_19, J_4, Line_2, Perm_6, Perms_4, Shoe} = Fun_19(Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_19, J_3, Line, Perm_6, Perms_4, Shoe),
            io:format("~ts~n", [(Line_2 ++ "\n")]),
            I_20 = (I_19 + 1),
            Fun_20_loop(Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_20, Perms_4, Shoe);
        _ -> {Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_19, Perms_4, Shoe}
    end
end,
{Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_20, Perms_4, Shoe} = Fun_20(Cards_2, Count2, FactNums_3, Fbn51s, G_2, I_19, Perms_4, Shoe),
        Fbn51 = [],
        I_21 = 0,
        Fun_21 = fun Fun_21_loop(Cards_2, Count2, FactNums_3, Fbn51, Fbn51s, G_2, I_21, Perms_4, Shoe) ->
    case (I_21 < 51) of
        true ->
            Fbn51_2 = lists:append(Fbn51, [randint((52 - I_21))]),
            I_22 = (I_21 + 1),
            Fun_21_loop(Cards_2, Count2, FactNums_3, Fbn51_2, Fbn51s, G_2, I_22, Perms_4, Shoe);
        _ -> {Cards_2, Count2, FactNums_3, Fbn51, Fbn51s, G_2, I_21, Perms_4, Shoe}
    end
end,
{Cards_2, Count2, FactNums_3, Fbn51_2, Fbn51s, G_2, I_22, Perms_4, Shoe} = Fun_21(Cards_2, Count2, FactNums_3, Fbn51, Fbn51s, G_2, I_21, Perms_4, Shoe),
        io:format("~ts~n", [joinints(Fbn51_2, ".")]),
        Perms_5 = maptoperms([Fbn51_2]),
        Line_3 = "",
        I_23 = 0,
        Fun_22 = fun Fun_22_loop(Cards_2, Count2, FactNums_3, Fbn51_2, Fbn51s, G_2, I_23, Line_3, Perms_5, Shoe) ->
    case (I_23 < length((case erlang:is_map(Perms_5) of true -> maps:get(0, Perms_5, nil); _ -> lists:nth(0 + 1, Perms_5) end))) of
        true ->
            Line_4 = (Line_3 ++ (case erlang:is_map(Cards_2) of true -> maps:get((case erlang:is_map((case erlang:is_map(Perms_5) of true -> maps:get(0, Perms_5, nil); _ -> lists:nth(0 + 1, Perms_5) end)) of true -> maps:get(I_23, (case erlang:is_map(Perms_5) of true -> maps:get(0, Perms_5, nil); _ -> lists:nth(0 + 1, Perms_5) end), nil); _ -> lists:nth(I_23 + 1, (case erlang:is_map(Perms_5) of true -> maps:get(0, Perms_5, nil); _ -> lists:nth(0 + 1, Perms_5) end)) end), Cards_2, nil); _ -> lists:nth((case erlang:is_map((case erlang:is_map(Perms_5) of true -> maps:get(0, Perms_5, nil); _ -> lists:nth(0 + 1, Perms_5) end)) of true -> maps:get(I_23, (case erlang:is_map(Perms_5) of true -> maps:get(0, Perms_5, nil); _ -> lists:nth(0 + 1, Perms_5) end), nil); _ -> lists:nth(I_23 + 1, (case erlang:is_map(Perms_5) of true -> maps:get(0, Perms_5, nil); _ -> lists:nth(0 + 1, Perms_5) end)) end) + 1, Cards_2) end)),
            I_24 = (I_23 + 1),
            Fun_22_loop(Cards_2, Count2, FactNums_3, Fbn51_2, Fbn51s, G_2, I_24, Line_4, Perms_5, Shoe);
        _ -> {Cards_2, Count2, FactNums_3, Fbn51_2, Fbn51s, G_2, I_23, Line_3, Perms_5, Shoe}
    end
end,
{Cards_2, Count2, FactNums_3, Fbn51_2, Fbn51s, G_2, I_24, Line_4, Perms_5, Shoe} = Fun_22(Cards_2, Count2, FactNums_3, Fbn51_2, Fbn51s, G_2, I_23, Line_3, Perms_5, Shoe),
        io:format("~ts~n", [Line_4]),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('seed', 1),
    main(),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
