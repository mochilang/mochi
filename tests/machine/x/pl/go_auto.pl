:- style_check(-singleton).
testpkg_add(A, B, R) :- R is A + B.
testpkg_pi(P) :- P is 3.14.
testpkg_answer(A) :- A is 42.

:- initialization(main, main).
main :-
    testpkg_add(2, 3, _V0),
    _V1 is _V0,
    writeln(_V1),
    testpkg_pi(_V2),
    writeln(_V2),
    testpkg_answer(_V3),
    writeln(_V3),
    true.
