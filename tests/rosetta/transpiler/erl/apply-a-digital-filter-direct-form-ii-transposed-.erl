#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, applyfilter/3]).

% Generated by Mochi transpiler v0.10.41 (cac7e4e2bd) on 2025-07-26 17:59 +0700


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

applyfilter(Input, A, B) ->
    try
        Out = [],
        Scale = (1 / lists:nth(0 + 1, A)),
        I = 0,
        Fun_3 = fun Fun_3_loop(A, B, I, Input, Out, Scale) ->
    case (I < length(Input)) of
        true ->
            Tmp = 0,
            J = 0,
            Fun = fun Fun_loop(A, B, I, Input, J, Out, Scale, Tmp) ->
    case ((J =< I) andalso (J < length(B))) of
        true ->
            Tmp_2 = (Tmp + (lists:nth(J + 1, B) * lists:nth((I - J) + 1, Input))),
            J_2 = (J + 1),
            Fun_loop(A, B, I, Input, J_2, Out, Scale, Tmp_2);
        _ -> {A, B, I, Input, J, Out, Scale, Tmp}
    end
end,
{A, B, I, Input, J_2, Out, Scale, Tmp_2} = Fun(A, B, I, Input, J, Out, Scale, Tmp),
            J_3 = 0,
            Fun_2 = fun Fun_2_loop(A, B, I, Input, J_3, Out, Scale, Tmp_2) ->
    case ((J_3 < I) andalso ((J_3 + 1) < length(A))) of
        true ->
            Tmp_3 = (Tmp_2 - (lists:nth((J_3 + 1) + 1, A) * lists:nth(((I - J_3) - 1) + 1, Out))),
            J_4 = (J_3 + 1),
            Fun_2_loop(A, B, I, Input, J_4, Out, Scale, Tmp_3);
        _ -> {A, B, I, Input, J_3, Out, Scale, Tmp_2}
    end
end,
{A, B, I, Input, J_4, Out, Scale, Tmp_3} = Fun_2(A, B, I, Input, J_3, Out, Scale, Tmp_2),
            Out_2 = lists:append(Out, [(Tmp_3 * Scale)]),
            I_2 = (I + 1),
            Fun_3_loop(A, B, I_2, Input, Out_2, Scale);
        _ -> {A, B, I, Input, Out, Scale}
    end
end,
{A, B, I_2, Input, Out_2, Scale} = Fun_3(A, B, I, Input, Out, Scale),
        Out_2
    catch {return, Ret} -> Ret end.

main(_) ->
    Start = mochi_now(),
    StartMem = erlang:memory(total),
    erlang:put('a', [1, -2.7756e-16, 0.33333333, -1.85e-17]),
    erlang:put('b', [0.16666667, 0.5, 0.5, 0.16666667]),
    erlang:put('sig', [-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589]),
    erlang:put('res', applyfilter(erlang:get('sig'), erlang:get('a'), erlang:get('b'))),
    erlang:put('k', 0),
    Fun_4 = fun Fun_4_loop() ->
    case (erlang:get('k') < length(erlang:get('res'))) of
        true ->
            io:format("~p~n", [lists:nth(erlang:get('k') + 1, erlang:get('res'))]),
            erlang:put('k', (erlang:get('k') + 1)),
            Fun_4_loop();
        _ -> {}
    end
end,
{} = Fun_4(),
    End = mochi_now(),
    EndMem = erlang:memory(total),
    DurationUs = (End - Start) div 1000,
    MemBytes = erlang:abs(EndMem - StartMem),
    io:format("{~n  \"duration_us\": ~p,~n  \"memory_bytes\": ~p,~n  \"name\": \"main\"~n}
", [DurationUs, MemBytes]).
