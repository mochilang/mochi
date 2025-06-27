:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).


            addtwonumbers(L1, L2, Res) :-
                catch(
                    (
        nb_setval(I, 0),
        nb_setval(J, 0),
        nb_setval(Carry, 0),
        nb_setval(Result, []),
        catch(
            (
                repeat,
                    nb_getval(I, _V0),
                    length(L1, _V1),
                    nb_getval(J, _V2),
                    length(L2, _V3),
                    nb_getval(Carry, _V4),
                    (((_V0 < _V1 ; _V2 < _V3) ; _V4 > 0) ->
                        catch(
                            (
                                nb_setval(X, 0),
                                nb_getval(I, _V5),
                                length(L1, _V6),
                                (_V5 < _V6 ->
                                    nb_getval(I, _V7),
                                    get_item(L1, _V7, _V8),
                                    nb_setval(X, _V8),
                                    nb_getval(I, _V9),
                                    _V10 is _V9 + 1,
                                    nb_setval(I, _V10)
                                ;
                                true
                                ),
                                nb_setval(Y, 0),
                                nb_getval(J, _V11),
                                length(L2, _V12),
                                (_V11 < _V12 ->
                                    nb_getval(J, _V13),
                                    get_item(L2, _V13, _V14),
                                    nb_setval(Y, _V14),
                                    nb_getval(J, _V15),
                                    _V16 is _V15 + 1,
                                    nb_setval(J, _V16)
                                ;
                                true
                                ),
                                nb_getval(X, _V17),
                                nb_getval(Y, _V18),
                                _V20 is _V17 + _V18,
                                nb_getval(Carry, _V19),
                                _V21 is _V20 + _V19,
                                Sum = _V21,
                                _V22 is Sum mod 10,
                                Digit = _V22,
                                _V23 is Sum // 10,
                                nb_setval(Carry, _V23),
                                nb_getval(Result, _V24),
                                append(_V24, [Digit], _V25),
                                nb_setval(Result, _V25),
                            ), continue, true),
                            fail
                        ; true)
                )
                , break, true)
                        ,
                        true
                    )
                    , return(_V26),
                        Res = _V26
                    )
                .
                addtwonumbers(L1, L2, Res) :-
                nb_getval(Result, _V27),
                Res = _V27.

test_example_1 :-
    addtwonumbers([2, 4, 3], [5, 6, 4], _V28),
    expect(_V28 =:= [7, 0, 8])
    ,
    true.

test_example_2 :-
    addtwonumbers([0], [0], _V29),
    expect(_V29 =:= [0])
    ,
    true.

test_example_3 :-
    addtwonumbers([9, 9, 9, 9, 9, 9, 9], [9, 9, 9, 9], _V30),
    expect(_V30 =:= [8, 9, 9, 9, 0, 0, 0, 1])
    ,
    true.

    main :-
    test_example_1,
    test_example_2,
    test_example_3
    .
:- initialization(main, main).
