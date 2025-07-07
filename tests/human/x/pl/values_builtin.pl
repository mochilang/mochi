:- style_check(-singleton).
    main :-
    dict_create(_V0, map, ['a'-1, 'b'-2, 'c'-3]),
    M = _V0,
    call(Values, M, _V1),
    write(_V1),
    nl
    .
:- initialization(main, main).
