:- initialization(main).
:- style_check(-singleton).

main :-
    X is 12,
    (X > 10 -> Msg = "yes" ; Msg = "no"),
    writeln(Msg).
