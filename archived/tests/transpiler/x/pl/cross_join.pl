:- style_check(-singleton).
:- initialization(main).

main :-
    Customers = [{id: 1, name: "Alice"}, {id: 2, name: "Bob"}, {id: 3, name: "Charlie"}],
    Orders = [{id: 100, customerId: 1, total: 250}, {id: 101, customerId: 2, total: 125}, {id: 102, customerId: 1, total: 300}],
    Result = [{orderId: 100, orderCustomerId: 1, pairedCustomerName: "Alice", orderTotal: 250}, {orderId: 100, orderCustomerId: 1, pairedCustomerName: "Bob", orderTotal: 250}, {orderId: 100, orderCustomerId: 1, pairedCustomerName: "Charlie", orderTotal: 250}, {orderId: 101, orderCustomerId: 2, pairedCustomerName: "Alice", orderTotal: 125}, {orderId: 101, orderCustomerId: 2, pairedCustomerName: "Bob", orderTotal: 125}, {orderId: 101, orderCustomerId: 2, pairedCustomerName: "Charlie", orderTotal: 125}, {orderId: 102, orderCustomerId: 1, pairedCustomerName: "Alice", orderTotal: 300}, {orderId: 102, orderCustomerId: 1, pairedCustomerName: "Bob", orderTotal: 300}, {orderId: 102, orderCustomerId: 1, pairedCustomerName: "Charlie", orderTotal: 300}],
    writeln("--- Cross Join: All order-customer pairs ---"),
    Entry = {orderId: 100, orderCustomerId: 1, pairedCustomerName: "Alice", orderTotal: 250},
    writeln("Order 100 (customerId: 1 , total: $ 250 ) paired with Alice"),
    Entry1 = {orderId: 100, orderCustomerId: 1, pairedCustomerName: "Bob", orderTotal: 250},
    writeln("Order 100 (customerId: 1 , total: $ 250 ) paired with Bob"),
    Entry2 = {orderId: 100, orderCustomerId: 1, pairedCustomerName: "Charlie", orderTotal: 250},
    writeln("Order 100 (customerId: 1 , total: $ 250 ) paired with Charlie"),
    Entry3 = {orderId: 101, orderCustomerId: 2, pairedCustomerName: "Alice", orderTotal: 125},
    writeln("Order 101 (customerId: 2 , total: $ 125 ) paired with Alice"),
    Entry4 = {orderId: 101, orderCustomerId: 2, pairedCustomerName: "Bob", orderTotal: 125},
    writeln("Order 101 (customerId: 2 , total: $ 125 ) paired with Bob"),
    Entry5 = {orderId: 101, orderCustomerId: 2, pairedCustomerName: "Charlie", orderTotal: 125},
    writeln("Order 101 (customerId: 2 , total: $ 125 ) paired with Charlie"),
    Entry6 = {orderId: 102, orderCustomerId: 1, pairedCustomerName: "Alice", orderTotal: 300},
    writeln("Order 102 (customerId: 1 , total: $ 300 ) paired with Alice"),
    Entry7 = {orderId: 102, orderCustomerId: 1, pairedCustomerName: "Bob", orderTotal: 300},
    writeln("Order 102 (customerId: 1 , total: $ 300 ) paired with Bob"),
    Entry8 = {orderId: 102, orderCustomerId: 1, pairedCustomerName: "Charlie", orderTotal: 300},
    writeln("Order 102 (customerId: 1 , total: $ 300 ) paired with Charlie").
