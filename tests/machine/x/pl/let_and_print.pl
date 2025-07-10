:- style_check(-singleton).
:- initialization(main, main).
main :-
    A is 10,
    B is 20,
    _V0 is (A + B),
    write(_V0),
    nl,
    true.
