:- style_check(-singleton).
map_keys(Dict, Keys) :-
    dict_pairs(Dict, _, Pairs),
    findall(K, member(K-_, Pairs), Keys).


        main :-
    dict_create(_V0, map, ['a'-1, 'b'-2]),
    nb_setval(m, _V0),
    nb_getval(m, _V1),
    map_keys(_V1, _V2),
    catch(
        (
            member(K, _V2),
            catch(
                (
                    write(K),
                    nl,
                    true
                ), continue, true),
                fail
                ;
                true
            )
            , break, true),
            true
        .
:- initialization(main, main).
