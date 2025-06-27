:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).


                lengthoflongestsubstring(S, Res) :-
                    catch(
                        (
        length(S, _V0),
        N = _V0,
        nb_setval(Start, 0),
        nb_setval(Best, 0),
        nb_setval(I, 0),
        catch(
            (
                repeat,
                    nb_getval(I, _V1),
                    (_V1 < N ->
                        catch(
                            (
                                nb_getval(Start, _V2),
                                nb_setval(J, _V2),
                                catch(
                                    (
                                        repeat,
                                            nb_getval(J, _V3),
                                            nb_getval(I, _V4),
                                            (_V3 < _V4 ->
                                                catch(
                                                    (
                                                        nb_getval(J, _V5),
                                                        get_item(S, _V5, _V6),
                                                        nb_getval(I, _V7),
                                                        get_item(S, _V7, _V8),
                                                        (_V6 =:= _V8 ->
                                                            nb_getval(J, _V9),
                                                            _V10 is _V9 + 1,
                                                            nb_setval(Start, _V10),
                                                            throw(break)
                                                        ;
                                                        true
                                                        ),
                                                        nb_getval(J, _V11),
                                                        _V12 is _V11 + 1,
                                                        nb_setval(J, _V12),
                                                    ), continue, true),
                                                    fail
                                                ; true)
                                        )
                                        , break, true),
                                    nb_getval(I, _V13),
                                    nb_getval(Start, _V14),
                                    _V15 is _V13 - _V14,
                                    _V16 is _V15 + 1,
                                    Length = _V16,
                                    nb_getval(Best, _V17),
                                    (Length > _V17 ->
                                        nb_setval(Best, Length)
                                    ;
                                    true
                                    ),
                                    nb_getval(I, _V18),
                                    _V19 is _V18 + 1,
                                    nb_setval(I, _V19),
                                ), continue, true),
                                fail
                            ; true)
                    )
                    , break, true)
                            ,
                            true
                        )
                        , return(_V20),
                            Res = _V20
                        )
                    .
                    lengthoflongestsubstring(S, Res) :-
                    nb_getval(Best, _V21),
                    Res = _V21.

test_example_1 :-
    lengthoflongestsubstring("abcabcbb", _V22),
    expect(_V22 =:= 3)
    ,
    true.

test_example_2 :-
    lengthoflongestsubstring("bbbbb", _V23),
    expect(_V23 =:= 1)
    ,
    true.

test_example_3 :-
    lengthoflongestsubstring("pwwkew", _V24),
    expect(_V24 =:= 3)
    ,
    true.

test_empty_string :-
    lengthoflongestsubstring("", _V25),
    expect(_V25 =:= 0)
    ,
    true.

    main :-
    test_example_1,
    test_example_2,
    test_example_3,
    test_empty_string
    .
:- initialization(main, main).
