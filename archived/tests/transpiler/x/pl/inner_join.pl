:- style_check(-singleton).
:- initialization(main).

main :-
    Customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}, {id: 3, name: "Charlie"}],
    Orders = [{id: 100, customerId: 1, total: 250}, {id: 101, customerId: 2, total: 125}, {id: 102, customerId: 1, total: 300}, {id: 103, customerId: 4, total: 80}],
    Result = [{orderId: 100, customerName: "Alice", total: 250}, {orderId: 101, customerName: "Bob", total: 125}, {orderId: 102, customerName: "Alice", total: 300}],
    writeln("--- Orders with customer info ---"),
    Entry = {orderId: 100, customerName: "Alice", total: 250},
    writeln("Order 100 by Alice - $ 250"),
    Entry1 = {orderId: 101, customerName: "Bob", total: 125},
    writeln("Order 101 by Bob - $ 125"),
    Entry2 = {orderId: 102, customerName: "Alice", total: 300},
    writeln("Order 102 by Alice - $ 300").
