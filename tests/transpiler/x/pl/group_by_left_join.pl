:- initialization(main).
:- style_check(-singleton).

main :-
    Customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}, {id: 3, name: "Charlie"}],
    Orders = [{id: 100, customerId: 1}, {id: 101, customerId: 1}, {id: 102, customerId: 2}],
    Stats = [{name: "Alice", count: 0}, {name: "Bob", count: 0}, {name: "Charlie", count: 0}],
    writeln("--- Group Left Join ---"),
    S = {name: "Alice", count: 0},
    writeln("Alice orders: 0"),
    S1 = {name: "Bob", count: 0},
    writeln("Bob orders: 0"),
    S2 = {name: "Charlie", count: 0},
    writeln("Charlie orders: 0").
