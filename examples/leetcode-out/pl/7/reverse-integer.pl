:- style_check(-singleton).
expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).


            reverse(X, Res) :-
                catch(
                    (
        nb_setval(Sign, 1),
        nb_setval(N, X),
        nb_getval(N, _V0),
        (_V0 < 0 ->
            _V1 is -(1),
            nb_setval(Sign, _V1),
            nb_getval(N, _V2),
            _V3 is -(_V2),
            nb_setval(N, _V3)
        ;
        true
        ),
        nb_setval(Rev, 0),
        catch(
            (
                repeat,
                    nb_getval(N, _V4),
                    (_V4 \= 0 ->
                        catch(
                            (
                                nb_getval(N, _V5),
                                _V6 is _V5 mod 10,
                                Digit = _V6,
                                nb_getval(Rev, _V7),
                                _V8 is _V7 * 10,
                                _V9 is _V8 + Digit,
                                nb_setval(Rev, _V9),
                                nb_getval(N, _V10),
                                _V11 is _V10 // 10,
                                nb_setval(N, _V11),
                            ), continue, true),
                            fail
                        ; true)
                )
                , break, true),
            nb_getval(Rev, _V12),
            nb_getval(Sign, _V13),
            _V14 is _V12 * _V13,
            nb_setval(Rev, _V14),
            nb_getval(Rev, _V15),
            _V16 is -(2147483647),
            _V17 is _V16 - 1,
            nb_getval(Rev, _V18),
            ((_V15 < _V17 ; _V18 > 2147483647) ->
                throw(return(0))
            ;
            true
            )
                        ,
                        true
                    )
                    , return(_V19),
                        Res = _V19
                    )
                .
                reverse(X, Res) :-
                nb_getval(Rev, _V20),
                Res = _V20.

test_example_1 :-
    reverse(123, _V21),
    expect(_V21 =:= 321)
    ,
    true.

test_example_2 :-
    _V22 is -(123),
    reverse(_V22, _V23),
    _V24 is -(321),
    expect(_V23 =:= _V24)
    ,
    true.

test_example_3 :-
    reverse(120, _V25),
    expect(_V25 =:= 21)
    ,
    true.

test_overflow :-
    reverse(1534236469, _V26),
    expect(_V26 =:= 0)
    ,
    true.

    main :-
    test_example_1,
    test_example_2,
    test_example_3,
    test_overflow
    .
:- initialization(main, main).
