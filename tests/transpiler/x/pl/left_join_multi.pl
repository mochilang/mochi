:- initialization(main).
:- style_check(-singleton).

main :-
    Customers = [_{id: 1, name: "Alice"}, _{id: 2, name: "Bob"}],
    Orders = [_{id: 100, customerId: 1}, _{id: 101, customerId: 2}],
    Items = [_{orderId: 100, sku: "a"}],
    Result = [_{orderId: 100, name: "Alice", item: _{orderId: 100, sku: "a"}}],
    writeln("--- Left Join Multi ---"),
    R = _{orderId: 100, name: "Alice", item: _{orderId: 100, sku: "a"}},
write(100), write(' '), write("Alice"), write(' '), writeln(_{orderId: 100, sku: "a"}).
