:- style_check(-singleton).
:- initialization(main, main).
main :-
    X is 12,
    ((X @> 10) -> _V0 = "yes" ; _V0 = "no"),
    Msg = _V0,
    write(Msg),
    nl,
    true.
