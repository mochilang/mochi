:- initialization(main).
:- style_check(-singleton).
last([X], X).
last([_|T], X) :-
    last(T, X).
main :-
        last([1, 2, 3, 4], L),
        writeln(L).
