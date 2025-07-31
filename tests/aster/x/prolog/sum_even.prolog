:- initialization(main).
:- style_check(-singleton).
sum_even([], 0).
sum_even([H|T], S) :-
        0 is H mod 2,
            sum_even(T, S1),
        S is S1 + H.
sum_even([_|T], S) :-
    sum_even(T, S).
main :-
        sum_even([1, 2, 3, 4, 5, 6], S),
        writeln(S).
