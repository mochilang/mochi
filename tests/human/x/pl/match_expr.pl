:- initialization(main).
main :-
    X = 2,
    ( X =:= 1 -> Label='one'
    ; X =:= 2 -> Label='two'
    ; X =:= 3 -> Label='three'
    ; Label='unknown'
    ),
    writeln(Label),
    halt.
