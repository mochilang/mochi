:- initialization(main).
:- style_check(-singleton).
loop(N, N) :-
    writeln(N).
loop(I, N) :-
        I < N,
            writeln(I),
            I1 is I + 1,
        loop(I1, N).
main :-
    loop(1, 3).
