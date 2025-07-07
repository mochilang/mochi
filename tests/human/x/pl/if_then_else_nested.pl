:- initialization(main).
main :-
    X = 8,
    ( X > 10 -> Msg = 'big'
    ; X > 5  -> Msg = 'medium'
    ; Msg = 'small'
    ),
    writeln(Msg),
    halt.
