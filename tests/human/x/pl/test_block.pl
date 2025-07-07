:- style_check(-singleton).
expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).


test_addition_works :-
    _V0 is 1 + 2,
    X = _V0,
    expect(X = 3)    ,
    true.

    main :-
    write("ok"),
    nl,
    test_addition_works
    .
:- initialization(main, main).
