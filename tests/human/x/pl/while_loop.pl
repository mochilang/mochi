:- style_check(-singleton).
        main :-
    nb_setval(i, 0),
    catch(
        (
            repeat,
                nb_getval(i, _V0),
                (_V0 < 3 ->
                    catch(
                        (
                            nb_getval(i, _V1),
                            write(_V1),
                            nl,
                            nb_getval(i, _V2),
                            _V3 is _V2 + 1,
                            nb_setval(i, _V3),
                        ), continue, true),
                        fail
                    ; true)
            )
            , break, true)
        .
:- initialization(main, main).
