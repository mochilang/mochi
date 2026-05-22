:- initialization(main).
:- style_check(-singleton).

gcd(A, 0, A).
gcd(A, B, D) :-
    B > 0,
    R is A mod B,
    gcd(B, R, D).

main :-
    gcd(48, 18, G),
    writeln(G).
