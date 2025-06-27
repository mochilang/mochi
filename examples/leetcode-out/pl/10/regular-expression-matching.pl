:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


set_item(Container, Key, Val, Out) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), put_dict(A, Container, Val, Out).
set_item(List, Index, Val, Out) :-
    nth0(Index, List, _, Rest),
    nth0(Index, Out, Val, Rest).


expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).


                        ismatch(S, P, Res) :-
                            catch(
                                (
        length(S, _V0),
        M = _V0,
        length(P, _V1),
        N = _V1,
        nb_setval(Dp, []),
        nb_setval(I, 0),
        catch(
            (
                repeat,
                    nb_getval(I, _V2),
                    (_V2 =< M ->
                        catch(
                            (
                                nb_setval(Row, []),
                                nb_setval(J, 0),
                                catch(
                                    (
                                        repeat,
                                            nb_getval(J, _V3),
                                            (_V3 =< N ->
                                                catch(
                                                    (
                                                        nb_getval(Row, _V4),
                                                        append(_V4, [false], _V5),
                                                        nb_setval(Row, _V5),
                                                        nb_getval(J, _V6),
                                                        _V7 is _V6 + 1,
                                                        nb_setval(J, _V7),
                                                    ), continue, true),
                                                    fail
                                                ; true)
                                        )
                                        , break, true),
                                    nb_getval(Dp, _V8),
                                    nb_getval(Row, _V9),
                                    append(_V8, [_V9], _V10),
                                    nb_setval(Dp, _V10),
                                    nb_getval(I, _V11),
                                    _V12 is _V11 + 1,
                                    nb_setval(I, _V12),
                                ), continue, true),
                                fail
                            ; true)
                    )
                    , break, true),
                nb_getval(Dp, _V13),
                get_item(_V13, M, _V14),
                set_item(_V14, N, true, _V15),
                set_item(_V13, M, _V15, _V16),
                nb_setval(Dp, _V16),
                nb_setval(I2, M),
                catch(
                    (
                        repeat,
                            nb_getval(I2, _V17),
                            (_V17 >= 0 ->
                                catch(
                                    (
                                        _V18 is N - 1,
                                        nb_setval(J2, _V18),
                                        catch(
                                            (
                                                repeat,
                                                    nb_getval(J2, _V19),
                                                    (_V19 >= 0 ->
                                                        catch(
                                                            (
                                                                nb_setval(First, false),
                                                                nb_getval(I2, _V20),
                                                                (_V20 < M ->
                                                                    nb_getval(J2, _V21),
                                                                    get_item(P, _V21, _V22),
                                                                    nb_getval(I2, _V23),
                                                                    get_item(S, _V23, _V24),
                                                                    nb_getval(J2, _V25),
                                                                    get_item(P, _V25, _V26),
                                                                    ((_V22 =:= _V24 ; _V26 =:= ".") ->
                                                                        nb_setval(First, true)
                                                                    ;
                                                                    true
                                                                    )
                                                                ;
                                                                true
                                                                ),
                                                                nb_setval(Star, false),
                                                                nb_getval(J2, _V27),
                                                                _V28 is _V27 + 1,
                                                                (_V28 < N ->
                                                                    nb_getval(J2, _V29),
                                                                    _V30 is _V29 + 1,
                                                                    get_item(P, _V30, _V31),
                                                                    (_V31 =:= "*" ->
                                                                        nb_setval(Star, true)
                                                                    ;
                                                                    true
                                                                    )
                                                                ;
                                                                true
                                                                ),
                                                                nb_getval(Star, _V32),
                                                                (_V32 ->
                                                                    nb_setval(Ok, false),
                                                                    nb_getval(Dp, _V33),
                                                                    nb_getval(I2, _V34),
                                                                    get_item(_V33, _V34, _V35),
                                                                    nb_getval(J2, _V36),
                                                                    _V37 is _V36 + 2,
                                                                    get_item(_V35, _V37, _V38),
                                                                    (_V38 ->
                                                                        nb_setval(Ok, true)
                                                                    ;
                                                                        nb_getval(First, _V39),
                                                                        (_V39 ->
                                                                            nb_getval(Dp, _V40),
                                                                            nb_getval(I2, _V41),
                                                                            _V42 is _V41 + 1,
                                                                            get_item(_V40, _V42, _V43),
                                                                            nb_getval(J2, _V44),
                                                                            get_item(_V43, _V44, _V45),
                                                                            (_V45 ->
                                                                                nb_setval(Ok, true)
                                                                            ;
                                                                            true
                                                                            )
                                                                        ;
                                                                        true
                                                                        )
                                                                    ),
                                                                    nb_getval(Dp, _V49),
                                                                    nb_getval(I2, _V46),
                                                                    get_item(_V49, _V46, _V50),
                                                                    nb_getval(J2, _V47),
                                                                    nb_getval(Ok, _V48),
                                                                    set_item(_V50, _V47, _V48, _V51),
                                                                    set_item(_V49, _V46, _V51, _V52),
                                                                    nb_setval(Dp, _V52)
                                                                ;
                                                                    nb_setval(Ok, false),
                                                                    nb_getval(First, _V53),
                                                                    (_V53 ->
                                                                        nb_getval(Dp, _V54),
                                                                        nb_getval(I2, _V55),
                                                                        _V56 is _V55 + 1,
                                                                        get_item(_V54, _V56, _V57),
                                                                        nb_getval(J2, _V58),
                                                                        _V59 is _V58 + 1,
                                                                        get_item(_V57, _V59, _V60),
                                                                        (_V60 ->
                                                                            nb_setval(Ok, true)
                                                                        ;
                                                                        true
                                                                        )
                                                                    ;
                                                                    true
                                                                    ),
                                                                    nb_getval(Dp, _V64),
                                                                    nb_getval(I2, _V61),
                                                                    get_item(_V64, _V61, _V65),
                                                                    nb_getval(J2, _V62),
                                                                    nb_getval(Ok, _V63),
                                                                    set_item(_V65, _V62, _V63, _V66),
                                                                    set_item(_V64, _V61, _V66, _V67),
                                                                    nb_setval(Dp, _V67)
                                                                ),
                                                                nb_getval(J2, _V68),
                                                                _V69 is _V68 - 1,
                                                                nb_setval(J2, _V69),
                                                            ), continue, true),
                                                            fail
                                                        ; true)
                                                )
                                                , break, true),
                                            nb_getval(I2, _V70),
                                            _V71 is _V70 - 1,
                                            nb_setval(I2, _V71),
                                        ), continue, true),
                                        fail
                                    ; true)
                            )
                            , break, true)
                                    ,
                                    true
                                )
                                , return(_V72),
                                    Res = _V72
                                )
                            .
                            ismatch(S, P, Res) :-
                            nb_getval(Dp, _V73),
                            get_item(_V73, 0, _V74),
                            get_item(_V74, 0, _V75),
                            Res = _V75.

test_example_1 :-
    ismatch("aa", "a", _V76),
    expect(_V76 =:= false)
    ,
    true.

test_example_2 :-
    ismatch("aa", "a*", _V77),
    expect(_V77 =:= true)
    ,
    true.

test_example_3 :-
    ismatch("ab", ".*", _V78),
    expect(_V78 =:= true)
    ,
    true.

test_example_4 :-
    ismatch("aab", "c*a*b", _V79),
    expect(_V79 =:= true)
    ,
    true.

test_example_5 :-
    ismatch("mississippi", "mis*is*p*.", _V80),
    expect(_V80 =:= false)
    ,
    true.

    main :-
    test_example_1,
    test_example_2,
    test_example_3,
    test_example_4,
    test_example_5
    .
:- initialization(main, main).
