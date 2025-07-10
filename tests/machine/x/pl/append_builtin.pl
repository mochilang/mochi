:- style_check(-singleton).
:- initialization(main, main).
main :-
    A = [1, 2],
    append(A, [3], _V0),
    write(_V0),
    nl,
    true.
