:- style_check(-singleton).
go_get(_, _, _) :- throw(error('go ffi not implemented')).
go_call(_, _, _, _) :- throw(error('go ffi not implemented')).

testpkg_add(A, B, R) :- R is A + B.
testpkg_pi(P) :- P is 3.14.
testpkg_answer(A) :- A is 42.

:- initialization(main, main).
main :-
    testpkg_add(2, 3, _V0),
    _V1 is _V0,
    write(_V1),
    nl,
    testpkg_pi(_V2),
    write(_V2),
    nl,
    testpkg_answer(_V3),
    write(_V3),
    nl,
    true.
