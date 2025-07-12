:- style_check(-singleton).
math_pi(P) :- P is 3.141592653589793.
math_e(E) :- E is 2.718281828459045.
math_sqrt(X, R) :- R is sqrt(X).
math_pow(X, Y, R) :- R is X ** Y.
math_sin(X, R) :- R is sin(X).
math_log(X, R) :- R is log(X).

:- initialization(main, main).
main :-
    math_sqrt(16, _V0),
    _V1 is _V0,
    writeln(_V1),
    math_pi(_V2),
    writeln(_V2),
    true.
