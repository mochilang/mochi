:- initialization(main).
:- style_check(-singleton).

main :-
    M = _{a: 1, b: 2},
    writeln("{
  \"a\": 1,
  \"b\": 2
}").
