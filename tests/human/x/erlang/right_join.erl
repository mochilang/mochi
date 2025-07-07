#!/usr/bin/env escript
%% right_join.erl - manual translation of tests/vm/valid/right_join.mochi

main(_) ->
    Customers = [
        #{id => 1, name => "Alice"},
        #{id => 2, name => "Bob"},
        #{id => 3, name => "Charlie"},
        #{id => 4, name => "Diana"}
    ],
    Orders = [
        #{id => 100, customerId => 1, total => 250},
        #{id => 101, customerId => 2, total => 125},
        #{id => 102, customerId => 1, total => 300}
    ],
    Result = lists:foldl(fun(C,Acc) ->
                Matches = [O || O <- Orders, maps:get(customerId,O) =:= maps:get(id,C)],
                case Matches of
                    [] -> [#{customerName => maps:get(name,C), order => undefined}|Acc];
                    _ -> lists:foldl(fun(O,A) -> [#{customerName => maps:get(name,C), order => O}|A] end, Acc, Matches)
                end
            end, [], Customers),
    io:format("--- Right Join using syntax ---~n"),
    lists:foreach(fun(E) ->
        case maps:get(order,E) of
            undefined -> io:format("Customer ~s has no orders~n", [maps:get(customerName,E)]);
            O -> io:format("Customer ~s has order ~p - $~p~n", [maps:get(customerName,E), maps:get(id,O), maps:get(total,O)])
        end
    end, lists:reverse(Result)).
