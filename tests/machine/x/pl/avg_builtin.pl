:- style_check(-singleton).
:- initialization(main, main).
main :-
    sum_list([1, 2, 3], _V0),
    length([1, 2, 3], _V1),
    _V1 > 0,
    _V2 is _V0 / _V1,
    _V3 is _V2,
    write(_V3),
    nl,
    true.
