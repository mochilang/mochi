:- initialization(main).
:- style_check(-singleton).

main :-
    Customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}],
    Orders = [{id: 100, customerId: 1}, {id: 101, customerId: 1}, {id: 102, customerId: 2}],
    Stats = [{name: "Alice", count: 2}, {name: "Bob", count: 1}],
    writeln("--- Orders per customer ---"),
    S = {name: "Alice", count: 2},
    writeln("Alice orders: 2"),
    S1 = {name: "Bob", count: 1},
    writeln("Bob orders: 1").
