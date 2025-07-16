:- style_check(-singleton).
        p__lambda0(X, Res) :-
        _V0 is X + N,
        Res = _V0.

        makeadder(N, Res) :-
        Res = p__lambda0.

    main :-
    makeadder(10, _V1),
    Add10 = _V1,
    call(Add10, 7, _V2),
    write(_V2),
    nl
    .
:- initialization(main, main).
