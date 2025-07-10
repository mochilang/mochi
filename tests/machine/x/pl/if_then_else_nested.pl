:- style_check(-singleton).
:- initialization(main, main).
main :-
    X is 8,
    ((X > 5) -> _V0 = "medium" ; _V0 = "small"),
    ((X > 10) -> _V1 = "big" ; _V1 = _V0),
    Msg = _V1,
    write(Msg),
    nl,
    true.
