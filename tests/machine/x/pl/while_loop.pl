:- style_check(-singleton).
:- initialization(main, main).
main :-
    _V0 is 0,
    nb_setval(i, _V0),
    catch(
        (
            repeat,
            nb_getval(i, _V1),
            ((_V1 < 3) ->
                catch(
                    (
                        nb_getval(i, _V2),
                        write(_V2),
                        nl,
                        nb_getval(i, _V3),
                        _V4 is (_V3 + 1),
                        nb_setval(i, _V4),
                        true
                    ), continue, true),
                    fail
                ; true)
            ), break, true),
        true.
