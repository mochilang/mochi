#!/usr/bin/env escript
%% break_continue.erl - manual translation of tests/vm/valid/break_continue.mochi

loop([]) -> ok;
loop([N|Rest]) ->
    case N > 7 of
        true -> ok; %% break
        false ->
            case N rem 2 =:= 0 of
                true -> loop(Rest); %% continue
                false ->
                    io:format("odd number: ~p~n", [N]),
                    loop(Rest)
            end
    end.

main(_) ->
    loop([1,2,3,4,5,6,7,8,9]).
