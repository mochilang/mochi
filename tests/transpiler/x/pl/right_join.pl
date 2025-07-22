:- initialization(main).
:- style_check(-singleton).

main :-
    Customers = [_{id: 1, name: "Alice"}, _{id: 2, name: "Bob"}, _{id: 3, name: "Charlie"}, _{id: 4, name: "Diana"}],
    Orders = [_{id: 100, customerId: 1, total: 250}, _{id: 101, customerId: 2, total: 125}, _{id: 102, customerId: 1, total: 300}],
    Result = [_{customerName: "Alice", order: _{id: 100, customerId: 1, total: 250}}, _{customerName: "Alice", order: _{id: 102, customerId: 1, total: 300}}, _{customerName: "Bob", order: _{id: 101, customerId: 2, total: 125}}],
    writeln("--- Right Join using syntax ---"),
    Entry = _{customerName: "Alice", order: _{id: 100, customerId: 1, total: 250}},
    (_{id: 100, customerId: 1, total: 250} ->
write("Customer"), write(' '), write("Alice"), write(' '), write("has order"), write(' '), get_dict(id, get_dict(order, Entry, R), V5), write(V5), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Entry, R), V6), writeln(V6) ;
    writeln("Customer Alice has no orders")),
    Entry1 = _{customerName: "Alice", order: _{id: 102, customerId: 1, total: 300}},
    (_{id: 102, customerId: 1, total: 300} ->
write("Customer"), write(' '), write("Alice"), write(' '), write("has order"), write(' '), get_dict(id, get_dict(order, Entry1, R), V7), write(V7), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Entry1, R), V8), writeln(V8) ;
    writeln("Customer Alice has no orders")),
    Entry2 = _{customerName: "Bob", order: _{id: 101, customerId: 2, total: 125}},
    (_{id: 101, customerId: 2, total: 125} ->
write("Customer"), write(' '), write("Bob"), write(' '), write("has order"), write(' '), get_dict(id, get_dict(order, Entry2, R), V9), write(V9), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Entry2, R), V10), writeln(V10) ;
    writeln("Customer Bob has no orders")).
