:- initialization(main).
:- style_check(-singleton).

main :-
    X is 2,
    (2 =:= 1 -> Label = "one" ; (2 =:= 2 -> Label = "two" ; (2 =:= 3 -> Label = "three" ; Label = "unknown"))),
    writeln(Label).
