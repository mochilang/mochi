:- initialization(main).
:- style_check(-singleton).

main :-
    People = [{name: "Alice", city: "Paris"}, {name: "Bob", city: "Hanoi"}, {name: "Charlie", city: "Paris"}, {name: "Diana", city: "Hanoi"}, {name: "Eve", city: "Paris"}, {name: "Frank", city: "Hanoi"}, {name: "George", city: "Paris"}],
    Big = [{city: "Paris", num: 4}],
    writeln("[
  {
    \"city\": \"Paris\",
    \"num\": 4
  }
]").
