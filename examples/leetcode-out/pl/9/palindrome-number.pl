:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).


            ispalindrome(X, Res) :-
                catch(
                    (
        (X < 0 ->
            throw(return(false))
        ;
        true
        ),
        term_string(X, _V0),
        S = _V0,
        length(S, _V1),
        N = _V1,
        _V2 is N // 2,
        _V3 is _V2 - 1,
        catch(
            (
                between(0, _V3, I),
                catch(
                    (
                        get_item(S, I, _V4),
                        _V5 is N - 1,
                        _V6 is _V5 - I,
                        get_item(S, _V6, _V7),
                        (_V4 \= _V7 ->
                            throw(return(false))
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
                    , return(_V8),
                        Res = _V8
                    )
                .
                ispalindrome(X, Res) :-
                Res = true.

test_example_1 :-
    ispalindrome(121, _V9),
    expect(_V9 =:= true)
    ,
    true.

test_example_2 :-
    _V10 is -(121),
    ispalindrome(_V10, _V11),
    expect(_V11 =:= false)
    ,
    true.

test_example_3 :-
    ispalindrome(10, _V12),
    expect(_V12 =:= false)
    ,
    true.

test_zero :-
    ispalindrome(0, _V13),
    expect(_V13 =:= true)
    ,
    true.

    main :-
    test_example_1,
    test_example_2,
    test_example_3,
    test_zero
    .
:- initialization(main, main).
