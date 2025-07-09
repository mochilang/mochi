#!/usr/bin/env escript
% record_assign.erl - generated from record_assign.mochi

inc(C) ->
    C0 = C#{n => (maps:get(n, C) + 1)}.

main(_) ->
    C1 = #{"__name" => "Counter", n => 0},
    inc(C1),
    io:format("~p~n", [maps:get(n, C1)]).
