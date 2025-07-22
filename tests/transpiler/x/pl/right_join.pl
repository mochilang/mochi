:- initialization(main).
:- style_check(-singleton).

main :-
    Customers = [map{id: 1, name: "Alice"}, map{id: 2, name: "Bob"}, map{id: 3, name: "Charlie"}, map{id: 4, name: "Diana"}],
    Orders = [map{id: 100, customerId: 1, total: 250}, map{id: 101, customerId: 2, total: 125}, map{id: 102, customerId: 1, total: 300}],
    Result = [map{customerName: "Alice", order: map{id: 100, customerId: 1, total: 250}}, map{customerName: "Bob", order: map{id: 101, customerId: 2, total: 125}}, map{customerName: "Alice", order: map{id: 102, customerId: 1, total: 300}}],
    writeln("--- Right Join using syntax ---"),
    Entry = map{customerName: "Alice", order: map{id: 100, customerId: 1, total: 250}},
write("Customer"), write(' '), write("Alice"), write(' '), write("has order"), write(' '), get_dict(id, get_dict(order, Entry, R), V5), write(V5), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Entry, R), V6), writeln(V6),
    Entry1 = map{customerName: "Bob", order: map{id: 101, customerId: 2, total: 125}},
write("Customer"), write(' '), write("Bob"), write(' '), write("has order"), write(' '), get_dict(id, get_dict(order, Entry1, R), V7), write(V7), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Entry1, R), V8), writeln(V8),
    Entry2 = map{customerName: "Alice", order: map{id: 102, customerId: 1, total: 300}},
write("Customer"), write(' '), write("Alice"), write(' '), write("has order"), write(' '), get_dict(id, get_dict(order, Entry2, R), V9), write(V9), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Entry2, R), V10), writeln(V10).
