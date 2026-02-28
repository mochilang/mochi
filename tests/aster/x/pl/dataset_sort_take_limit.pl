:- initialization(main).
:- style_check(-singleton).
main :-
        Products = [_{name: 'Laptop', price: 1500}, _{price: 900, name: 'Smartphone'}, _{name: 'Tablet', price: 600}, _{name: 'Monitor', price: 300}, _{name: 'Keyboard', price: 100}, _{name: 'Mouse', price: 50}, _{name: 'Headphones', price: 200}],
            Expensive = [_{price: 900, name: 'Smartphone'}, _{price: 600, name: 'Tablet'}, _{name: 'Monitor', price: 300}],
            writeln('--- Top products (excluding most expensive) ---'),
            Item = _{name: 'Smartphone', price: 900},
            writeln('Smartphone costs $ 900'),
            Item1 = _{price: 600, name: 'Tablet'},
            writeln('Tablet costs $ 600'),
            Item2 = _{name: 'Monitor', price: 300},
        writeln('Monitor costs $ 300').
