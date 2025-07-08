:- initialization(main, main).
main :-
    A is (10 - 3),
    B is (2 + 2),
    writeln(A),
    ((A =:= 7) -> _V0 = true ; _V0 = false),
    writeln(_V0),
    ((B < 5) -> _V1 = true ; _V1 = false),
    writeln(_V1),
    true.
