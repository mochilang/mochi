:- style_check(-singleton).
:- initialization(main, main).
main :-
    dict_create(_V0, map, [a-1, b-2]),
    nb_setval(m, _V0),
    catch(
        (
            nb_getval(m, _V1),
            member(K, _V1),
                catch(
                    (
                        write(K),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
