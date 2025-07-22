:- initialization(main).
:- style_check(-singleton).

main :-
    Products = [_{name: "Laptop", price: 1500}, _{name: "Smartphone", price: 900}, _{name: "Tablet", price: 600}, _{name: "Monitor", price: 300}, _{name: "Keyboard", price: 100}, _{name: "Mouse", price: 50}, _{name: "Headphones", price: 200}],
    Expensive = [_{name: "Smartphone", price: 900}, _{name: "Tablet", price: 600}, _{name: "Monitor", price: 300}],
    writeln("--- Top products (excluding most expensive) ---"),
    Item = _{name: "Smartphone", price: 900},
    writeln("Smartphone costs $ 900"),
    Item1 = _{name: "Tablet", price: 600},
    writeln("Tablet costs $ 600"),
    Item2 = _{name: "Monitor", price: 300},
    writeln("Monitor costs $ 300").
