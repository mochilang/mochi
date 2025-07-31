:- initialization(main).
:- style_check(-singleton).
sum_pairs([], []).
sum_pairs([A|[B|T]], [S|R]) :-
        S is A + B,
        sum_pairs(T, R).
main :-
        sum_pairs([1, 2, 3, 4], R),
        writeln(R).
