:- style_check(-singleton).
:- initialization(main).

main :-
    X is 12,
    (X > 10 -> Msg = "yes" ; Msg = "no"),
    writeln(Msg).
