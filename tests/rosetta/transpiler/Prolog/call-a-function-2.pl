:- initialization(main).
:- style_check(-singleton).

f(R) :-
    Return1 = [0, 0],
    R = Return1.

g(A, B, R) :-
    Return1 is 0,
    R = Return1.

h(S, Nums, R) :-
    Return is 0,
    R = Return.

main(R) :-
    f(_),
    g(1, 2, _),
    Res = [0, 0],
    g(0, 0, _),
    g(g(1, 2), 3, _),
    Return is 0,
    R = Return.

main :-
    main(_).
