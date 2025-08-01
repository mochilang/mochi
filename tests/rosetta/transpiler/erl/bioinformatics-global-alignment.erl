#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, padleft/2, indexoffrom/3, containsstr/2, distinct/1, permutations/1, headtailoverlap/2, deduplicate/1, joinall/1, shortestcommonsuperstring/1, printcounts/1, main/0]).

% Generated by Mochi transpiler v0.10.40 (7bf46dbf08) on 2025-07-25 14:27 +0700


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

indexoffrom(S_2, Ch, Start) ->
    try
        I = Start,
        Fun_2 = fun Fun_2_loop(Ch, I, S_2, Start) ->
    case (I < length(S_2)) of
        true ->
            case (string:substr(S_2, I + 1, ((I + 1) - I)) == Ch) of
        true -> throw({return, I});
        _ -> ok
    end,
            I_2 = (I + 1),
            Fun_2_loop(Ch, I_2, S_2, Start);
        _ -> {Ch, I, S_2, Start}
    end
end,
{Ch, I_2, S_2, Start} = Fun_2(Ch, I, S_2, Start),
        -1
    catch {return, Ret} -> Ret end.

containsstr(S_3, Sub) ->
    try
        I_3 = 0,
        Sl = length(S_3),
        Subl = length(Sub),
        Fun_3 = fun Fun_3_loop(I_3, S_3, Sl, Sub, Subl) ->
    case (I_3 =< (Sl - Subl)) of
        true ->
            case (string:substr(S_3, I_3 + 1, ((I_3 + Subl) - I_3)) == Sub) of
        true -> throw({return, true});
        _ -> ok
    end,
            I_4 = (I_3 + 1),
            Fun_3_loop(I_4, S_3, Sl, Sub, Subl);
        _ -> {I_3, S_3, Sl, Sub, Subl}
    end
end,
{I_4, S_3, Sl, Sub, Subl} = Fun_3(I_3, S_3, Sl, Sub, Subl),
        false
    catch {return, Ret} -> Ret end.

distinct(Slist) ->
    try
        Res_3 = [],
        Fun_4 = fun Fun_4_loop(List, Res_3, Slist) ->
    case List of
        [] -> {Res_3, Slist};
        [S_4|List_rest] ->
        try
            Found = false,
            Fun_5 = fun Fun_5_loop(List, Found, Res_3, S_4, Slist) ->
    case List of
        [] -> {Found, Res_3, S_4, Slist};
        [R|List_rest] ->
        try
            case (R == S_4) of
        true -> Found_2 = true,
            throw(break),
            Found_3 = Found_2;
        _ -> Found_3 = Found
    end,
            Fun_5_loop(List_rest, Found_3, Res_3, S_4, Slist)
        catch
            {continue, C0, C1, C2, C3} -> Fun_5_loop(List_rest, C0, C1, C2, C3);
            {break, B0, B1, B2, B3} -> {B0, B1, B2, B3};
            break -> {Found, Res_3, S_4, Slist}
        end
    end
end,
{Found_3, Res_3, S_4, Slist} = Fun_5(Res_3, Found, Res_3, S_4, Slist),
            case not Found_3 of
        true -> Res_4 = lists:append(Res_3, [S_4]),
            Res_5 = Res_4;
        _ -> Res_5 = Res_3
    end,
            Fun_4_loop(List_rest, Res_5, Slist)
        catch
            {continue, C0, C1} -> Fun_4_loop(List_rest, C0, C1);
            {break, B0, B1} -> {B0, B1};
            break -> {Res_3, Slist}
        end
    end
end,
{Res_5, Slist} = Fun_4(Slist, Res_3, Slist),
        Res_5
    catch {return, Ret} -> Ret end.

permutations(Xs) ->
    try
        case (length(Xs) =< 1) of
        true -> throw({return, [Xs]});
        _ -> ok
    end,
        Res_6 = [],
        I_5 = 0,
        Fun_9 = fun Fun_9_loop(I_5, Res_6, Xs) ->
    case (I_5 < length(Xs)) of
        true ->
            Rest = [],
            J = 0,
            Fun_6 = fun Fun_6_loop(I_5, J, Res_6, Rest, Xs) ->
    case (J < length(Xs)) of
        true ->
            case (J /= I_5) of
        true -> Rest_2 = lists:append(Rest, [lists:nth(J + 1, Xs)]),
            Rest_3 = Rest_2;
        _ -> Rest_3 = Rest
    end,
            J_2 = (J + 1),
            Fun_6_loop(I_5, J_2, Res_6, Rest_3, Xs);
        _ -> {I_5, J, Res_6, Rest, Xs}
    end
end,
{I_5, J_2, Res_6, Rest_3, Xs} = Fun_6(I_5, J, Res_6, Rest, Xs),
            Subs = permutations(Rest_3),
            Fun_7 = fun Fun_7_loop(List, I_5, J_2, Res_6, Rest_3, Subs, Xs) ->
    case List of
        [] -> {I_5, J_2, Res_6, Rest_3, Subs, Xs};
        [P|List_rest] ->
            Perm = [lists:nth(I_5 + 1, Xs)],
            K = 0,
            Fun_8 = fun Fun_8_loop(I_5, J_2, K, P, Perm, Res_6, Rest_3, Subs, Xs) ->
    case (K < length(P)) of
        true ->
            Perm_2 = lists:append(Perm, [lists:nth(K + 1, P)]),
            K_2 = (K + 1),
            Fun_8_loop(I_5, J_2, K_2, P, Perm_2, Res_6, Rest_3, Subs, Xs);
        _ -> {I_5, J_2, K, P, Perm, Res_6, Rest_3, Subs, Xs}
    end
end,
{I_5, J_2, K_2, P, Perm_2, Res_6, Rest_3, Subs, Xs} = Fun_8(I_5, J_2, K, P, Perm, Res_6, Rest_3, Subs, Xs),
            Res_7 = lists:append(Res_6, [Perm_2]),
            Fun_7_loop(List_rest, I_5, J_2, Res_7, Rest_3, Subs, Xs)
    end
end,
{I_5, J_2, Res_7, Rest_3, Subs, Xs} = Fun_7(Subs, I_5, J_2, Res_6, Rest_3, Subs, Xs),
            I_6 = (I_5 + 1),
            Fun_9_loop(I_6, Res_7, Xs);
        _ -> {I_5, Res_6, Xs}
    end
end,
{I_6, Res_7, Xs} = Fun_9(I_5, Res_6, Xs),
        Res_7
    catch {return, Ret} -> Ret end.

headtailoverlap(S1, S2) ->
    try
        Start_2 = 0,
        Fun_10 = fun Fun_10_loop(S1, S2, Start_2) ->
    case true of
        true ->
            Ix = indexoffrom(S1, string:substr(S2, 0 + 1, (1 - 0)), Start_2),
            case (Ix == (0 - 1)) of
        true -> throw({return, 0});
        _ -> ok
    end,
            Start_3 = Ix,
            case (string:substr(S2, 0 + 1, ((length(S1) - Start_3) - 0)) == string:substr(S1, Start_3 + 1, (length(S1) - Start_3))) of
        true -> throw({return, (length(S1) - Start_3)});
        _ -> ok
    end,
            Start_4 = (Start_3 + 1),
            Fun_10_loop(S1, S2, Start_4);
        _ -> {S1, S2, Start_2}
    end
end,
{S1, S2, Start_4} = Fun_10(S1, S2, Start_2),
        nil
    catch {return, Ret} -> Ret end.

deduplicate(Slist_2) ->
    try
        Arr = distinct(Slist_2),
        Filtered = [],
        I_7 = 0,
        Fun_12 = fun Fun_12_loop(Arr, Filtered, I_7, Slist_2) ->
    case (I_7 < length(Arr)) of
        true ->
            try
                S1_2 = lists:nth(I_7 + 1, Arr),
                Within = false,
                J_3 = 0,
                Fun_11 = fun Fun_11_loop(Arr, Filtered, I_7, J_3, S1_2, Slist_2, Within) ->
    case (J_3 < length(Arr)) of
        true ->
            try
                case ((J_3 /= I_7) andalso containsstr(lists:nth(J_3 + 1, Arr), S1_2)) of
        true -> Within_2 = true,
            throw(break),
            Within_3 = Within_2;
        _ -> Within_3 = Within
    end,
                J_4 = (J_3 + 1),
                Fun_11_loop(Arr, Filtered, I_7, J_4, S1_2, Slist_2, Within_3)
            catch
                {continue, C0, C1, C2, C3, C4, C5, C6} -> Fun_11_loop(C0, C1, C2, C3, C4, C5, C6);
                break -> {Arr, Filtered, I_7, J_3, S1_2, Slist_2, Within}
            end;
        _ -> {Arr, Filtered, I_7, J_3, S1_2, Slist_2, Within}
    end
end,
{Arr, Filtered, I_7, J_4, S1_2, Slist_2, Within_3} = Fun_11(Arr, Filtered, I_7, J_3, S1_2, Slist_2, Within),
                case not Within_3 of
        true -> Filtered_2 = lists:append(Filtered, [S1_2]),
            Filtered_3 = Filtered_2;
        _ -> Filtered_3 = Filtered
    end,
                I_8 = (I_7 + 1),
                Fun_12_loop(Arr, Filtered_3, I_8, Slist_2)
            catch
                {continue, C0, C1, C2, C3} -> Fun_12_loop(C0, C1, C2, C3);
                break -> {Arr, Filtered, I_7, Slist_2}
            end;
        _ -> {Arr, Filtered, I_7, Slist_2}
    end
end,
{Arr, Filtered_3, I_8, Slist_2} = Fun_12(Arr, Filtered, I_7, Slist_2),
        Filtered_3
    catch {return, Ret} -> Ret end.

joinall(Ss) ->
    try
        Out = "",
        Fun_13 = fun Fun_13_loop(List, Out, Ss) ->
    case List of
        [] -> {Out, Ss};
        [S_5|List_rest] ->
            Out_2 = (Out ++ S_5),
            Fun_13_loop(List_rest, Out_2, Ss)
    end
end,
{Out_2, Ss} = Fun_13(Ss, Out, Ss),
        Out_2
    catch {return, Ret} -> Ret end.

shortestcommonsuperstring(Slist_3) ->
    try
        Ss_2 = deduplicate(Slist_3),
        Shortest = joinall(Ss_2),
        Perms = permutations(Ss_2),
        Idx = 0,
        Fun_15 = fun Fun_15_loop(Idx, Perms, Shortest, Slist_3, Ss_2) ->
    case (Idx < length(Perms)) of
        true ->
            Perm_3 = lists:nth(Idx + 1, Perms),
            Sup = lists:nth(0 + 1, Perm_3),
            I_9 = 0,
            Fun_14 = fun Fun_14_loop(I_9, Idx, Perm_3, Perms, Shortest, Slist_3, Ss_2, Sup) ->
    case (I_9 < (length(Ss_2) - 1)) of
        true ->
            Ov = headtailoverlap(lists:nth(I_9 + 1, Perm_3), lists:nth((I_9 + 1) + 1, Perm_3)),
            Sup_2 = (Sup ++ string:substr(lists:nth((I_9 + 1) + 1, Perm_3), Ov + 1, (length(lists:nth((I_9 + 1) + 1, Perm_3)) - Ov))),
            I_10 = (I_9 + 1),
            Fun_14_loop(I_10, Idx, Perm_3, Perms, Shortest, Slist_3, Ss_2, Sup_2);
        _ -> {I_9, Idx, Perm_3, Perms, Shortest, Slist_3, Ss_2, Sup}
    end
end,
{I_10, Idx, Perm_3, Perms, Shortest, Slist_3, Ss_2, Sup_2} = Fun_14(I_9, Idx, Perm_3, Perms, Shortest, Slist_3, Ss_2, Sup),
            case (length(Sup_2) < length(Shortest)) of
        true -> Shortest_2 = Sup_2,
            Shortest_3 = Shortest_2;
        _ -> Shortest_3 = Shortest
    end,
            Idx_2 = (Idx + 1),
            Fun_15_loop(Idx_2, Perms, Shortest_3, Slist_3, Ss_2);
        _ -> {Idx, Perms, Shortest, Slist_3, Ss_2}
    end
end,
{Idx_2, Perms, Shortest_3, Slist_3, Ss_2} = Fun_15(Idx, Perms, Shortest, Slist_3, Ss_2),
        Shortest_3
    catch {return, Ret} -> Ret end.

printcounts(Seq) ->
    try
        A = 0,
        C = 0,
        G = 0,
        T = 0,
        I_11 = 0,
        Fun_16 = fun Fun_16_loop(A, C, G, I_11, Seq, T) ->
    case (I_11 < length(Seq)) of
        true ->
            Ch_2 = string:substr(Seq, I_11 + 1, ((I_11 + 1) - I_11)),
            case (Ch_2 == "A") of
        true -> A_2 = (A + 1),
            A_3 = A_2,
            C_4 = C,
            G_5 = G,
            T_6 = T;
        _ -> case (Ch_2 == "C") of
        true -> C_2 = (C + 1),
            C_3 = C_2,
            G_4 = G,
            T_5 = T;
        _ -> case (Ch_2 == "G") of
        true -> G_2 = (G + 1),
            G_3 = G_2,
            T_4 = T;
        _ -> case (Ch_2 == "T") of
        true -> T_2 = (T + 1),
            T_3 = T_2;
        _ -> T_3 = T
    end,
            G_3 = G,
            T_4 = T_3
    end,
            C_3 = C,
            G_4 = G_3,
            T_5 = T_4
    end,
            A_3 = A,
            C_4 = C_3,
            G_5 = G_4,
            T_6 = T_5
    end,
            I_12 = (I_11 + 1),
            Fun_16_loop(A_3, C_4, G_5, I_12, Seq, T_6);
        _ -> {A, C, G, I_11, Seq, T}
    end
end,
{A_3, C_4, G_5, I_12, Seq, T_6} = Fun_16(A, C, G, I_11, Seq, T),
        Total = length(Seq),
        io:format("~ts~n", [(("\nNucleotide counts for " ++ Seq) ++ ":\n")]),
        io:format("~p~n", [(padleft("A", 10) ++ padleft(lists:flatten(io_lib:format("~p", [A_3])), 12))]),
        io:format("~p~n", [(padleft("C", 10) ++ padleft(lists:flatten(io_lib:format("~p", [C_4])), 12))]),
        io:format("~p~n", [(padleft("G", 10) ++ padleft(lists:flatten(io_lib:format("~p", [G_5])), 12))]),
        io:format("~p~n", [(padleft("T", 10) ++ padleft(lists:flatten(io_lib:format("~p", [T_6])), 12))]),
        io:format("~p~n", [(padleft("Other", 10) ++ padleft(lists:flatten(io_lib:format("~p", [(Total - (((A_3 + C_4) + G_5) + T_6))])), 12))]),
        io:format("~ts~n", ["  ____________________"]),
        io:format("~p~n", [(padleft("Total length", 14) ++ padleft(lists:flatten(io_lib:format("~p", [Total])), 8))]),
        nil
    catch {return, Ret} -> Ret end.

main() ->
    try
        Tests = [["TA", "AAG", "TA", "GAA", "TA"], ["CATTAGGG", "ATTAG", "GGG", "TA"], ["AAGAUGGA", "GGAGCGCAUC", "AUCGCAAUAAGGA"], ["ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT", "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT", "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC", "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC", "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT", "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"]],
        Fun_17 = fun Fun_17_loop(List, Tests) ->
    case List of
        [] -> {Tests};
        [Seqs|List_rest] ->
            Scs = shortestcommonsuperstring(Seqs),
            printcounts(Scs),
            Fun_17_loop(List_rest, Tests)
    end
end,
{Tests} = Fun_17(Tests, Tests),
        nil
    catch {return, Ret} -> Ret end.

main(_) ->
    Start = mochi_now(),
    StartMem = erlang:memory(total),
    main(),
    End = mochi_now(),
    EndMem = erlang:memory(total),
    DurationUs = (End - Start) div 1000,
    MemBytes = abs(EndMem - StartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
