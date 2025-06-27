:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


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


                    convert(S, NumRows, Res) :-
                        catch(
                            (
        length(S, _V0),
        ((NumRows =< 1 ; NumRows >= _V0) ->
            throw(return(S))
        ;
        true
        ),
        nb_setval(Rows, []),
        nb_setval(I, 0),
        catch(
            (
                repeat,
                    nb_getval(I, _V1),
                    (_V1 < NumRows ->
                        catch(
                            (
                                nb_getval(Rows, _V2),
                                append(_V2, [""], _V3),
                                nb_setval(Rows, _V3),
                                nb_getval(I, _V4),
                                _V5 is _V4 + 1,
                                nb_setval(I, _V5),
                            ), continue, true),
                            fail
                        ; true)
                )
                , break, true),
            nb_setval(Curr, 0),
            nb_setval(Step, 1),
            to_list(S, _V6),
            catch(
                (
                    member(Ch, _V6),
                    catch(
                        (
                            nb_getval(Rows, _V12),
                            nb_getval(Curr, _V7),
                            nb_getval(Rows, _V8),
                            nb_getval(Curr, _V9),
                            get_item(_V8, _V9, _V10),
                            _V11 is _V10 + Ch,
                            set_item(_V12, _V7, _V11, _V13),
                            nb_setval(Rows, _V13),
                            nb_getval(Curr, _V14),
                            (_V14 =:= 0 ->
                                nb_setval(Step, 1)
                            ;
                            nb_getval(Curr, _V15),
                            _V16 is NumRows - 1,
                            (_V15 =:= _V16 ->
                                _V17 is -(1),
                                nb_setval(Step, _V17)
                            ;
                            true
                            )
                            ),
                            nb_getval(Curr, _V18),
                            nb_getval(Step, _V19),
                            _V20 is _V18 + _V19,
                            nb_setval(Curr, _V20),
                            true
                        ), continue, true),
                        fail
                        ;
                        true
                    )
                    , break, true),
                    true,
                nb_setval(Result, ""),
                nb_getval(Rows, _V21),
                to_list(_V21, _V22),
                catch(
                    (
                        member(Row, _V22),
                        catch(
                            (
                                nb_getval(Result, _V23),
                                _V24 is _V23 + Row,
                                nb_setval(Result, _V24),
                                true
                            ), continue, true),
                            fail
                            ;
                            true
                        )
                        , break, true),
                        true
                                ,
                                true
                            )
                            , return(_V25),
                                Res = _V25
                            )
                        .
                        convert(S, NumRows, Res) :-
                        nb_getval(Result, _V26),
                        Res = _V26.

test_example_1 :-
    convert("PAYPALISHIRING", 3, _V27),
    expect(_V27 =:= "PAHNAPLSIIGYIR")
    ,
    true.

test_example_2 :-
    convert("PAYPALISHIRING", 4, _V28),
    expect(_V28 =:= "PINALSIGYAHRPI")
    ,
    true.

test_single_row :-
    convert("A", 1, _V29),
    expect(_V29 =:= "A")
    ,
    true.

    main :-
    test_example_1,
    test_example_2,
    test_single_row
    .
:- initialization(main, main).
