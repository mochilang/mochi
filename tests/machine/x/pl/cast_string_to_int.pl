:- style_check(-singleton).
:- initialization(main, main).
main :-
    number_string(_V0, "1995"),
    _V1 is _V0,
    write(_V1),
    nl,
    true.
