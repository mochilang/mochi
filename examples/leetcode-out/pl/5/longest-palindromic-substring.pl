:- style_check(-singleton).
slice(Str, I, J, Out) :-
    string(Str), !,
    Len is J - I,
    sub_string(Str, I, Len, _, Out).
slice(List, I, J, Out) :-
    length(Prefix, I),
    append(Prefix, Rest, List),
    Len is J - I,
    length(Out, Len),
    append(Out, _, Rest).


get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).


            expand(S, Left, Right, Res) :-
                catch(
                    (
        nb_setval(L, Left),
        nb_setval(R, Right),
        length(S, _V0),
        N = _V0,
        catch(
            (
                repeat,
                    nb_getval(L, _V1),
                    nb_getval(R, _V2),
                    ((_V1 >= 0, _V2 < N) ->
                        catch(
                            (
                                nb_getval(L, _V3),
                                get_item(S, _V3, _V4),
                                nb_getval(R, _V5),
                                get_item(S, _V5, _V6),
                                (_V4 \= _V6 ->
                                    throw(break)
                                ;
                                true
                                ),
                                nb_getval(L, _V7),
                                _V8 is _V7 - 1,
                                nb_setval(L, _V8),
                                nb_getval(R, _V9),
                                _V10 is _V9 + 1,
                                nb_setval(R, _V10),
                            ), continue, true),
                            fail
                        ; true)
                )
                , break, true)
                        ,
                        true
                    )
                    , return(_V11),
                        Res = _V11
                    )
                .
                expand(S, Left, Right, Res) :-
                nb_getval(R, _V12),
                nb_getval(L, _V13),
                _V14 is _V12 - _V13,
                _V15 is _V14 - 1,
                Res = _V15.

            longestpalindrome(S, Res) :-
                catch(
                    (
        length(S, _V16),
        (_V16 =< 1 ->
            throw(return(S))
        ;
        true
        ),
        nb_setval(Start, 0),
        nb_setval(End, 0),
        length(S, _V17),
        N = _V17,
        _V18 is N - 1,
        catch(
            (
                between(0, _V18, I),
                catch(
                    (
                        expand(S, I, I, _V19),
                        Len1 = _V19,
                        _V20 is I + 1,
                        expand(S, I, _V20, _V21),
                        Len2 = _V21,
                        nb_setval(L, Len1),
                        (Len2 > Len1 ->
                            nb_setval(L, Len2)
                        ;
                        true
                        ),
                        nb_getval(L, _V22),
                        nb_getval(End, _V23),
                        nb_getval(Start, _V24),
                        _V25 is _V23 - _V24,
                        (_V22 > _V25 ->
                            nb_getval(L, _V26),
                            _V27 is _V26 - 1,
                            _V28 is _V27 // 2,
                            _V29 is I - _V28,
                            nb_setval(Start, _V29),
                            nb_getval(L, _V30),
                            _V31 is _V30 // 2,
                            _V32 is I + _V31,
                            nb_setval(End, _V32)
                        ;
                        true
                        ),
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
                    , return(_V33),
                        Res = _V33
                    )
                .
                longestpalindrome(S, Res) :-
                nb_getval(Start, _V34),
                nb_getval(End, _V35),
                _V36 is _V35 + 1,
                slice(S, _V34, _V36, _V37),
                Res = _V37.

test_example_1 :-
    longestpalindrome("babad", _V38),
    Ans = _V38,
    expect((Ans =:= "bab" ; Ans =:= "aba"))
    ,
    true.

test_example_2 :-
    longestpalindrome("cbbd", _V39),
    expect(_V39 =:= "bb")
    ,
    true.

test_single_char :-
    longestpalindrome("a", _V40),
    expect(_V40 =:= "a")
    ,
    true.

test_two_chars :-
    longestpalindrome("ac", _V41),
    Ans = _V41,
    expect((Ans =:= "a" ; Ans =:= "c"))
    ,
    true.

    main :-
    test_example_1,
    test_example_2,
    test_single_char,
    test_two_chars
    .
:- initialization(main, main).
