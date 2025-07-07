:- style_check(-singleton).
        add(A, B, Res) :-
        _V0 is A + B,
        Res = _V0.

    main :-
    add(5, _V1),
    Add5 = _V1,
    call(Add5, 3, _V2),
    write(_V2),
    nl
    .
:- initialization(main, main).
