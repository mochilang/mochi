:- initialization(main).
:- style_check(-singleton).

main :-
    Customers = [_{id: 1, name: "Alice"}, _{id: 2, name: "Bob"}, _{id: 3, name: "Charlie"}, _{id: 4, name: "Diana"}],
    Orders = [_{id: 100, customerId: 1, total: 250}, _{id: 101, customerId: 2, total: 125}, _{id: 102, customerId: 1, total: 300}, _{id: 103, customerId: 5, total: 80}],
    Result = [_{order: _{id: 100, customerId: 1, total: 250}, customer: _{id: 1, name: "Alice"}}, _{order: _{id: 101, customerId: 2, total: 125}, customer: _{id: 2, name: "Bob"}}, _{order: _{id: 102, customerId: 1, total: 300}, customer: _{id: 1, name: "Alice"}}],
    writeln("--- Outer Join using syntax ---"),
    Row = _{order: _{id: 100, customerId: 1, total: 250}, customer: _{id: 1, name: "Alice"}},
    (_{id: 100, customerId: 1, total: 250} ->
    (_{id: 1, name: "Alice"} ->
write("Order"), write(' '), get_dict(id, get_dict(order, Row, R), V5), write(V5), write(' '), write("by"), write(' '), get_dict(name, get_dict(customer, Row, R), V6), write(V6), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Row, R), V7), writeln(V7) ;
write("Order"), write(' '), get_dict(id, get_dict(order, Row, R), V5), write(V5), write(' '), write("by"), write(' '), write("Unknown"), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Row, R), V6), writeln(V6)) ;
write("Customer"), write(' '), get_dict(name, get_dict(customer, Row, R), V5), write(V5), write(' '), writeln("has no orders")),
    Row1 = _{order: _{id: 101, customerId: 2, total: 125}, customer: _{id: 2, name: "Bob"}},
    (_{id: 101, customerId: 2, total: 125} ->
    (_{id: 2, name: "Bob"} ->
write("Order"), write(' '), get_dict(id, get_dict(order, Row1, R), V7), write(V7), write(' '), write("by"), write(' '), get_dict(name, get_dict(customer, Row1, R), V8), write(V8), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Row1, R), V9), writeln(V9) ;
write("Order"), write(' '), get_dict(id, get_dict(order, Row1, R), V7), write(V7), write(' '), write("by"), write(' '), write("Unknown"), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Row1, R), V8), writeln(V8)) ;
write("Customer"), write(' '), get_dict(name, get_dict(customer, Row1, R), V7), write(V7), write(' '), writeln("has no orders")),
    Row2 = _{order: _{id: 102, customerId: 1, total: 300}, customer: _{id: 1, name: "Alice"}},
    (_{id: 102, customerId: 1, total: 300} ->
    (_{id: 1, name: "Alice"} ->
write("Order"), write(' '), get_dict(id, get_dict(order, Row2, R), V9), write(V9), write(' '), write("by"), write(' '), get_dict(name, get_dict(customer, Row2, R), V10), write(V10), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Row2, R), V11), writeln(V11) ;
write("Order"), write(' '), get_dict(id, get_dict(order, Row2, R), V9), write(V9), write(' '), write("by"), write(' '), write("Unknown"), write(' '), write("- $"), write(' '), get_dict(total, get_dict(order, Row2, R), V10), writeln(V10)) ;
write("Customer"), write(' '), get_dict(name, get_dict(customer, Row2, R), V9), write(V9), write(' '), writeln("has no orders")).
