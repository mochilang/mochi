#!/usr/bin/env escript
%% record_assign.erl - manual translation of tests/vm/valid/record_assign.mochi

-record(counter, {n}).

main(_) ->
    C0 = #counter{n=0},
    C1 = C0#counter{n=C0#counter.n + 1},
    io:format("~p~n", [C1#counter.n]).
