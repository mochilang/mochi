:- style_check(-singleton).
:- initialization(main, main).
main :-
    X is 8,
    Msg = ((X > 10) -> "big" ; ((X > 5) -> "medium" ; "small")),
    writeln(Msg),
    true.
