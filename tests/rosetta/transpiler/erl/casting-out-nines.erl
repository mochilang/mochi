#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, parseintbase/2, inttobase/2, subset/3]).

% Generated by Mochi transpiler v0.10.42 (bbaa8b9136) on 2025-07-28 00:21 +0700


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


mochi_not(X) ->
    case X of
        true -> false;
        false -> true;
        nil -> true;
        _ -> false
    end.

parseintbase(S, Base) ->
    try
        Digits = "0123456789abcdefghijklmnopqrstuvwxyz",
        N = 0,
        I = 0,
        Fun_2 = fun Fun_2_loop(Base, Digits, I, N, S) ->
    case (I < length(S)) of
        true ->
            try
                J = 0,
                V = 0,
                Fun = fun Fun_loop(Base, Digits, I, J, N, S, V) ->
    case (J < length(Digits)) of
        true ->
            try
                case (string:substr(Digits, J + 1, ((J + 1) - J)) == string:substr(S, I + 1, ((I + 1) - I))) of
        true -> V_2 = J,
            throw(break),
            V_3 = V_2;
        _ -> V_3 = V
    end,
                J_2 = (J + 1),
                Fun_loop(Base, Digits, I, J_2, N, S, V_3)
            catch
                {continue, C0, C1, C2, C3, C4, C5, C6} -> Fun_loop(C0, C1, C2, C3, C4, C5, C6);
                break -> {Base, Digits, I, J, N, S, V}
            end;
        _ -> {Base, Digits, I, J, N, S, V}
    end
end,
{Base, Digits, I, J_2, N, S, V_3} = Fun(Base, Digits, I, J, N, S, V),
                N_2 = ((N * Base) + V_3),
                I_2 = (I + 1),
                Fun_2_loop(Base, Digits, I_2, N_2, S)
            catch
                {continue, C0, C1, C2, C3, C4} -> Fun_2_loop(C0, C1, C2, C3, C4);
                break -> {Base, Digits, I, N, S}
            end;
        _ -> {Base, Digits, I, N, S}
    end
end,
{Base, Digits, I_2, N_2, S} = Fun_2(Base, Digits, I, N, S),
        N_2
    catch {return, Ret} -> Ret end.

inttobase(N_3, Base_2) ->
    try
        Digits_2 = "0123456789abcdefghijklmnopqrstuvwxyz",
        case (N_3 == 0) of
        true -> throw({return, "0"});
        _ -> ok
    end,
        Out = "",
        V_4 = N_3,
        Fun_3 = fun Fun_3_loop(Base_2, Digits_2, N_3, Out, V_4) ->
    case (V_4 > 0) of
        true ->
            D = (V_4 rem Base_2),
            Out_2 = (string:substr(Digits_2, D + 1, ((D + 1) - D)) ++ Out),
            V_5 = (V_4 div Base_2),
            Fun_3_loop(Base_2, Digits_2, N_3, Out_2, V_5);
        _ -> {Base_2, Digits_2, N_3, Out, V_4}
    end
end,
{Base_2, Digits_2, N_3, Out_2, V_5} = Fun_3(Base_2, Digits_2, N_3, Out, V_4),
        Out_2
    catch {return, Ret} -> Ret end.

subset(Base_3, Begin, End) ->
    try
        B = parseintbase(Begin, Base_3),
        E = parseintbase(End, Base_3),
        Out_3 = [],
        K = B,
        Fun_4 = fun Fun_4_loop(B, Base_3, Begin, E, End, K, Out_3) ->
    case (K =< E) of
        true ->
            Ks = inttobase(K, Base_3),
            Mod = (Base_3 - 1),
            R1 = (parseintbase(Ks, Base_3) rem Mod),
            R2 = ((parseintbase(Ks, Base_3) * parseintbase(Ks, Base_3)) rem Mod),
            case (R1 == R2) of
        true -> Out_4 = lists:append(Out_3, [Ks]),
            Out_5 = Out_4;
        _ -> Out_5 = Out_3
    end,
            K_2 = (K + 1),
            Fun_4_loop(B, Base_3, Begin, E, End, K_2, Out_5);
        _ -> {B, Base_3, Begin, E, End, K, Out_3}
    end
end,
{B, Base_3, Begin, E, End, K_2, Out_5} = Fun_4(B, Base_3, Begin, E, End, K, Out_3),
        Out_5
    catch {return, Ret} -> Ret end.

main(_) ->
    BenchStart = mochi_now(),
    BenchStartMem = erlang:memory(total),
    erlang:put('testCases', [#{"base" => 10, "begin" => "1", "end" => "100", "kaprekar" => ["1", "9", "45", "55", "99"]}, #{"base" => 17, "begin" => "10", "end" => "gg", "kaprekar" => ["3d", "d4", "gg"]}]),
    erlang:put('idx', 0),
    Fun_7 = fun Fun_7_loop() ->
    case (erlang:get('idx') < length(erlang:get('testCases'))) of
        true ->
            try
                Tc = lists:nth(erlang:get('idx') + 1, erlang:get('testCases')),
                io:format("~ts~n", [(((((("\nTest case base = " ++ lists:flatten(io_lib:format("~p", [maps:get("base", Tc, nil)]))) ++ ", begin = ") ++ maps:get("begin", Tc, nil)) ++ ", end = ") ++ maps:get("end", Tc, nil)) ++ ":")]),
                S_2 = subset(maps:get("base", Tc, nil), maps:get("begin", Tc, nil), maps:get("end", Tc, nil)),
                io:format("~ts~n", [("Subset:  " ++ lists:flatten(io_lib:format("~p", [S_2])))]),
                io:format("~ts~n", [("Kaprekar:" ++ lists:flatten(io_lib:format("~p", [maps:get("kaprekar", Tc, nil)])))]),
                Sx = 0,
                Valid = true,
                I_3 = 0,
                Fun_6 = fun Fun_6_loop(I_3, S_2, Sx, Tc, Valid) ->
    case (I_3 < length(maps:get("kaprekar", Tc, nil))) of
        true ->
            try
                K_3 = lists:nth(I_3 + 1, maps:get("kaprekar", Tc, nil)),
                Found = false,
                Fun_5 = fun Fun_5_loop(Found, I_3, K_3, S_2, Sx, Tc, Valid) ->
    case (Sx < length(S_2)) of
        true ->
            try
                case (lists:nth(Sx + 1, S_2) == K_3) of
        true -> Found_2 = true,
            Sx_2 = (Sx + 1),
            throw(break),
            Found_3 = Found_2,
            Sx_3 = Sx_2;
        _ -> Found_3 = Found,
            Sx_3 = Sx
    end,
                Sx_4 = (Sx_3 + 1),
                Fun_5_loop(Found_3, I_3, K_3, S_2, Sx_4, Tc, Valid)
            catch
                {continue, C0, C1, C2, C3, C4, C5, C6} -> Fun_5_loop(C0, C1, C2, C3, C4, C5, C6);
                break -> {Found, I_3, K_3, S_2, Sx, Tc, Valid}
            end;
        _ -> {Found, I_3, K_3, S_2, Sx, Tc, Valid}
    end
end,
{Found_3, I_3, K_3, S_2, Sx_4, Tc, Valid} = Fun_5(Found, I_3, K_3, S_2, Sx, Tc, Valid),
                case mochi_not(Found_3) of
        true -> io:format("~ts~n", [(("Fail:" ++ K_3) ++ " not in subset")]),
            Valid_2 = false,
            throw(break),
            Valid_3 = Valid_2;
        _ -> Valid_3 = Valid
    end,
                I_4 = (I_3 + 1),
                Fun_6_loop(I_4, S_2, Sx_4, Tc, Valid_3)
            catch
                {continue, C0, C1, C2, C3, C4} -> Fun_6_loop(C0, C1, C2, C3, C4);
                break -> {I_3, S_2, Sx, Tc, Valid}
            end;
        _ -> {I_3, S_2, Sx, Tc, Valid}
    end
end,
{I_4, S_2, Sx_4, Tc, Valid_3} = Fun_6(I_3, S_2, Sx, Tc, Valid),
                case (Valid_3 /= nil) of
        true -> io:format("~ts~n", ["Valid subset."]);
        _ -> ok
    end,
                erlang:put('idx', (erlang:get('idx') + 1)),
                Fun_7_loop()
            catch
                {continue} -> Fun_7_loop();
                break -> {}
            end;
        _ -> {}
    end
end,
{} = Fun_7(),
    BenchEnd = mochi_now(),
    BenchEndMem = erlang:memory(total),
    DurationUs = (BenchEnd - BenchStart) div 1000,
    MemBytes = erlang:abs(BenchEndMem - BenchStartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
