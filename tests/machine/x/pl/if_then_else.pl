:- style_check(-singleton).
:- initialization(main, main).
main :-
    X is 12,
    Msg = ((X > 10) -> "yes" ; "no"),
    writeln(Msg),
    true.
