:- style_check(-singleton).
        inc(X, Res) :-
        _V0 is X + K,
        Res = _V0.

    main :-
    K = 2,
    inc(3, _V1),
    write(_V1),
    nl
    .
:- initialization(main, main).
