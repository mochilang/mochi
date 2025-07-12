:- style_check(-singleton).
:- initialization(main, main).
main :-
    X is 2,
    (X == 3 -> _V0 = "three" ; _V0 = "unknown"),
    (X == 2 -> _V1 = "two" ; _V1 = _V0),
    (X == 1 -> _V2 = "one" ; _V2 = _V1),
    Label = _V2,
    writeln(Label),
    true.
