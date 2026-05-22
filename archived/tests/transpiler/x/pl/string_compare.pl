:- initialization(main).
:- style_check(-singleton).

main :-
    ("a" @< "b" -> writeln(true) ; writeln(false)),
    ("a" @=< "a" -> writeln(true) ; writeln(false)),
    ("b" @> "a" -> writeln(true) ; writeln(false)),
    ("b" @>= "b" -> writeln(true) ; writeln(false)).
