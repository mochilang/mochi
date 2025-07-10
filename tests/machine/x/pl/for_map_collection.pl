:- style_check(-singleton).
:- initialization(main, main).
main :-
    dict_create(_V0, map, [a-1, b-2]),
    M = _V0,
    catch(
        (
            member(K, M),
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
