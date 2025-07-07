#!/usr/bin/env escript
%% outer_join.erl - manual translation of tests/vm/valid/outer_join.mochi

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
        #{id => 102, customerId => 1, total => 300},
        #{id => 103, customerId => 5, total => 80}
    ],
    OrderRows = [#{order => O, customer => find_customer(maps:get(customerId,O), Customers)} || O <- Orders],
    CustomerRows = [#{order => undefined, customer => C} ||
                       C <- Customers,
                       not lists:any(fun(O) -> maps:get(customerId,O) =:= maps:get(id,C) end, Orders)],
    Result = OrderRows ++ CustomerRows,
    io:format("--- Outer Join using syntax ---~n"),
    lists:foreach(fun(Row) ->
        case Row of
            #{order := O, customer := C} when is_map(O), is_map(C) ->
                io:format("Order ~p by ~s - $~p~n", [maps:get(id,O), maps:get(name,C), maps:get(total,O)]);
            #{order := O, customer := undefined} ->
                io:format("Order ~p by Unknown - $~p~n", [maps:get(id,O), maps:get(total,O)]);
            #{order := undefined, customer := C} ->
                io:format("Customer ~s has no orders~n", [maps:get(name,C)])
        end
    end, Result).

find_customer(Id, Cs) ->
    case [C || C <- Cs, maps:get(id,C) =:= Id] of
        [H|_] -> H;
        [] -> undefined
    end.
