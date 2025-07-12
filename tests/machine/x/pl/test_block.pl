:- style_check(-singleton).
expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).

    test_addition_works :-
        X is (1 + 2),
        expect((X == 3)),
        true.
    
:- initialization(main, main).
main :-
    test_addition_works,
    writeln("ok"),
    true.
