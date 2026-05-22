:- initialization(main).
:- style_check(-singleton).

main :-
    People = [map{age: 30, email: "alice@example.com", name: "Alice"}, map{age: 15, email: "bob@example.com", name: "Bob"}, map{age: 20, email: "charlie@example.com", name: "Charlie"}],
    Adults = [map{name: "Alice", email: "alice@example.com"}, map{name: "Charlie", email: "charlie@example.com"}],
    A = map{name: "Alice", email: "alice@example.com"},
    writeln("Alice alice@example.com"),
    A1 = map{name: "Charlie", email: "charlie@example.com"},
    writeln("Charlie charlie@example.com").
