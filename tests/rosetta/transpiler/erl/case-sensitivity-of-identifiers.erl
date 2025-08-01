#!/usr/bin/env escript
-module(main).
-compile([nowarn_shadow_vars, nowarn_unused_vars]).
-export([main/1, main/0]).

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

main() ->
    try
        Pkg_dog = "Salt",
        Dog = "Pepper",
        Pkg_DOG = "Mustard",
        PackageSees = fun PackageSees(D1, D2, D3) ->
    try
        io:format("~ts~n", [((((("Package sees: " ++ D1) ++ " ") ++ D2) ++ " ") ++ D3)]),
        #{"pkg_dog" => true, "Dog" => true, "pkg_DOG" => true}
    catch {return, Ret} -> Ret end
end,
        D = PackageSees(Pkg_dog, Dog, Pkg_DOG),
        io:format("~ts~n", [(("There are " ++ lists:flatten(io_lib:format("~p", [maps:size(D)]))) ++ " dogs.\n")]),
        Dog_1 = "Benjamin",
        D_2 = PackageSees(Pkg_dog, Dog, Pkg_DOG),
        io:format("~ts~n", [((((("Main sees:   " ++ Dog_1) ++ " ") ++ Dog) ++ " ") ++ Pkg_DOG)]),
        D_3 = maps:put("dog", true, D_2),
        D_4 = maps:put("Dog", true, D_3),
        D_5 = maps:put("pkg_DOG", true, D_4),
        io:format("~ts~n", [(("There are " ++ lists:flatten(io_lib:format("~p", [maps:size(D_5)]))) ++ " dogs.\n")]),
        Dog_2 = "Samba",
        D_6 = PackageSees(Pkg_dog, "Samba", Pkg_DOG),
        io:format("~ts~n", [((((("Main sees:   " ++ Dog_1) ++ " ") ++ "Samba") ++ " ") ++ Pkg_DOG)]),
        D_7 = maps:put("dog", true, D_6),
        D_8 = maps:put("Dog", true, D_7),
        D_9 = maps:put("pkg_DOG", true, D_8),
        io:format("~ts~n", [(("There are " ++ lists:flatten(io_lib:format("~p", [maps:size(D_9)]))) ++ " dogs.\n")]),
        DOG = "Bernie",
        D_10 = PackageSees(Pkg_dog, "Samba", Pkg_DOG),
        io:format("~ts~n", [((((("Main sees:   " ++ Dog_1) ++ " ") ++ "Samba") ++ " ") ++ DOG)]),
        D_11 = maps:put("dog", true, D_10),
        D_12 = maps:put("Dog", true, D_11),
        D_13 = maps:put("pkg_DOG", true, D_12),
        D_14 = maps:put("DOG", true, D_13),
        io:format("~ts~n", [(("There are " ++ lists:flatten(io_lib:format("~p", [maps:size(D_14)]))) ++ " dogs.")]),
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
