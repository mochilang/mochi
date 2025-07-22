:- initialization(main).
:- style_check(-singleton).

main :-
    Customers = [_{id: 1, name: "Alice"}, _{id: 2, name: "Bob"}],
    Orders = [_{id: 100, customerId: 1, total: 250}, _{id: 101, customerId: 3, total: 80}],
    Result = [_{orderId: 100, customer: _{id: 1, name: "Alice"}, total: 250}, _{orderId: 101, customer: _{}, total: 80}],
    writeln("--- Left Join ---"),
    Entry = _{orderId: 100, customer: _{id: 1, name: "Alice"}, total: 250},
write("Order"), write(' '), write(100), write(' '), write("customer"), write(' '), write(_{id: 1, name: "Alice"}), write(' '), write("total"), write(' '), writeln(250),
    Entry1 = _{orderId: 101, customer: _{}, total: 80},
write("Order"), write(' '), write(101), write(' '), write("customer"), write(' '), write(_{}), write(' '), write("total"), write(' '), writeln(80).
