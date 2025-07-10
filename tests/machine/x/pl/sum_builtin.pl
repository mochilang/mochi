:- style_check(-singleton).
:- initialization(main, main).
main :-
    sum_list([1, 2, 3], _V0),
    _V1 is _V0,
    write(_V1),
    nl,
    true.
