:- initialization(main).
main :-
    X = 12,
    (X > 10 -> Msg = 'yes' ; Msg = 'no'),
    writeln(Msg),
    halt.
