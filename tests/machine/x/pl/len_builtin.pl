:- style_check(-singleton).
:- initialization(main, main).
main :-
    length([1, 2, 3], _V0),
    _V1 is _V0,
    writeln(_V1),
    true.
