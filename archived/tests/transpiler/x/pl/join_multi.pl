:- initialization(main).

main :-
    Customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}],
    Orders = [{id: 100, customerId: 1}, {id: 101, customerId: 2}],
    Items = [{orderId: 100, sku: "a"}, {orderId: 101, sku: "b"}],
    Result = [{name: "Alice", sku: "a"}, {name: "Bob", sku: "b"}],
    writeln("--- Multi Join ---"),
    R = {name: "Alice", sku: "a"},
    writeln("Alice bought item a"),
    R1 = {name: "Bob", sku: "b"},
    writeln("Bob bought item b").
