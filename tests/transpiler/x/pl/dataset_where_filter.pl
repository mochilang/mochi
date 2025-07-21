:- style_check(-singleton).
:- initialization(main).

main :-
    People = [{name: "Alice", age: 30}, {name: "Bob", age: 15}, {name: "Charlie", age: 65}, {name: "Diana", age: 45}],
    Adults = [{name: "Alice", age: 30, is_senior: false}, {name: "Charlie", age: 65, is_senior: true}, {name: "Diana", age: 45, is_senior: false}],
    writeln("--- Adults ---"),
    Person = {name: "Alice", age: 30, is_senior: false},
    writeln("Alice is 30 "),
    Person1 = {name: "Charlie", age: 65, is_senior: true},
    writeln("Charlie is 65  (senior)"),
    Person2 = {name: "Diana", age: 45, is_senior: false},
    writeln("Diana is 45 ").
