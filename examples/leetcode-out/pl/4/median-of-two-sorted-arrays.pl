:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).


            findmediansortedarrays(Nums1, Nums2, Res) :-
                catch(
                    (
        nb_setval(Merged, []),
        nb_setval(I, 0),
        nb_setval(J, 0),
        catch(
            (
                repeat,
                    nb_getval(I, _V0),
                    length(Nums1, _V1),
                    nb_getval(J, _V2),
                    length(Nums2, _V3),
                    ((_V0 < _V1 ; _V2 < _V3) ->
                        catch(
                            (
                                nb_getval(J, _V4),
                                length(Nums2, _V5),
                                (_V4 >= _V5 ->
                                    nb_getval(Merged, _V6),
                                    nb_getval(I, _V7),
                                    get_item(Nums1, _V7, _V8),
                                    append(_V6, [_V8], _V9),
                                    nb_setval(Merged, _V9),
                                    nb_getval(I, _V10),
                                    _V11 is _V10 + 1,
                                    nb_setval(I, _V11)
                                ;
                                nb_getval(I, _V12),
                                length(Nums1, _V13),
                                (_V12 >= _V13 ->
                                    nb_getval(Merged, _V14),
                                    nb_getval(J, _V15),
                                    get_item(Nums2, _V15, _V16),
                                    append(_V14, [_V16], _V17),
                                    nb_setval(Merged, _V17),
                                    nb_getval(J, _V18),
                                    _V19 is _V18 + 1,
                                    nb_setval(J, _V19)
                                ;
                                nb_getval(I, _V20),
                                get_item(Nums1, _V20, _V21),
                                nb_getval(J, _V22),
                                get_item(Nums2, _V22, _V23),
                                (_V21 =< _V23 ->
                                    nb_getval(Merged, _V24),
                                    nb_getval(I, _V25),
                                    get_item(Nums1, _V25, _V26),
                                    append(_V24, [_V26], _V27),
                                    nb_setval(Merged, _V27),
                                    nb_getval(I, _V28),
                                    _V29 is _V28 + 1,
                                    nb_setval(I, _V29)
                                ;
                                    nb_getval(Merged, _V30),
                                    nb_getval(J, _V31),
                                    get_item(Nums2, _V31, _V32),
                                    append(_V30, [_V32], _V33),
                                    nb_setval(Merged, _V33),
                                    nb_getval(J, _V34),
                                    _V35 is _V34 + 1,
                                    nb_setval(J, _V35)
                                )
                                )
                                ),
                            ), continue, true),
                            fail
                        ; true)
                )
                , break, true),
            nb_getval(Merged, _V36),
            length(_V36, _V37),
            Total = _V37,
            _V38 is Total mod 2,
            (_V38 =:= 1 ->
                nb_getval(Merged, _V39),
                _V40 is Total // 2,
                get_item(_V39, _V40, _V41),
                throw(return(_V41))
            ;
            true
            ),
            nb_getval(Merged, _V42),
            _V43 is Total // 2,
            _V44 is _V43 - 1,
            get_item(_V42, _V44, _V45),
            Mid1 = _V45,
            nb_getval(Merged, _V46),
            _V47 is Total // 2,
            get_item(_V46, _V47, _V48),
            Mid2 = _V48
                        ,
                        true
                    )
                    , return(_V49),
                        Res = _V49
                    )
                .
                findmediansortedarrays(Nums1, Nums2, Res) :-
                _V50 is Mid1 + Mid2,
                _V51 is _V50 // 2,
                Res = _V51.

test_example_1 :-
    findmediansortedarrays([1, 3], [2], _V52),
    expect(_V52 =:= 2)
    ,
    true.

test_example_2 :-
    findmediansortedarrays([1, 2], [3, 4], _V53),
    expect(_V53 =:= 2.5)
    ,
    true.

test_empty_first :-
    findmediansortedarrays([], [1], _V54),
    expect(_V54 =:= 1)
    ,
    true.

test_empty_second :-
    findmediansortedarrays([2], [], _V55),
    expect(_V55 =:= 2)
    ,
    true.

    main :-
    test_example_1,
    test_example_2,
    test_empty_first,
    test_empty_second
    .
:- initialization(main, main).
