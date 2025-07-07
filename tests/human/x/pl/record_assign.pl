:- style_check(-singleton).

        inc(C, Res) :-
            catch(
                (
        get_dict(n, C, _V0),
        _V1 is _V0 + 1,
        C = _V1
                    ,
                    true
                )
                , return(_V2),
                    Res = _V2
                )
            .

    main :-
    dict_create(_V3, p_counter, [n-0]),
    nb_setval(c, _V3),
    nb_getval(c, _V4),
    inc(_V4, _V5),
    nb_getval(c, _V6),
    get_dict(n, _V6, _V7),
    write(_V7),
    nl
    .
:- initialization(main, main).
