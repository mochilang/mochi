:- initialization(main).
:- style_check(-singleton).

countdown(0) :- writeln(0).
countdown(N) :-
    N > 0,
    writeln(N),
    N1 is N - 1,
    countdown(N1).

main :-
    countdown(3).
