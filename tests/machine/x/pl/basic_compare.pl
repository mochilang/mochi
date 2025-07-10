:- style_check(-singleton).
:- initialization(main, main).
main :-
    A is (10 - 3),
    B is (2 + 2),
    write(A),
    nl,
    ((A == 7) -> _V0 = true ; _V0 = false),
    write(_V0),
    nl,
    ((B < 5) -> _V1 = true ; _V1 = false),
    write(_V1),
    nl,
    true.
