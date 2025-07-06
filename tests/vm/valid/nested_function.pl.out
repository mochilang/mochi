:- style_check(-singleton).
        outer(X, Res) :-
            catch(
                (
                inner(Y, Res) :-
                _V0 is X + Y,
                Res = _V0.
                    ,
                    true
                )
                , return(_V1),
                    Res = _V1
                )
            .
            outer(X, Res) :-
            inner(5, _V2),
            Res = _V2.

    main :-
    outer(3, _V3),
    write(_V3),
    nl
    .
:- initialization(main, main).
