:- initialization(main).
:- style_check(-singleton).
print_evens(I, N) :-
        =<(I, N),
            0 is I mod 2,
            writeln(I),
            I1 is I + 1,
        print_evens(I1, N).
print_evens(I, N) :-
        I < N,
            I1 is I + 1,
        print_evens(I1, N).
print_evens(_, _).
main :-
    print_evens(0, 4).
