:- style_check(-singleton).
math_pi(P) :- P is 3.141592653589793.
math_e(E) :- E is 2.718281828459045.
math_sqrt(X, R) :- R is sqrt(X).
math_pow(X, Y, R) :- R is X ** Y.
math_sin(X, R) :- R is sin(X).
math_log(X, R) :- R is log(X).

:- initialization(main, main).
main :-
    R is 3,
    math_pi(_V0),
    math_pow(R, 2, _V1),
    Area is (_V0 * _V1),
    math_sqrt(49, _V2),
    Root is _V2,
    math_pi(_V3),
    math_sin((_V3 / 4), _V4),
    Sin45 is _V4,
    math_e(_V5),
    math_log(_V5, _V6),
    Log_e is _V6,
    write("Circle area with r ="),
    write(' '),
    write(R),
    write(' '),
    ("=>" -> _V7 = true ; _V7 = false),
    write(_V7),
    write(' '),
    write(Area),
    nl,
    write("Square root of 49:"),
    write(' '),
    write(Root),
    nl,
    write("sin(π/4):"),
    write(' '),
    write(Sin45),
    nl,
    write("log(e):"),
    write(' '),
    write(Log_e),
    nl,
    true.
