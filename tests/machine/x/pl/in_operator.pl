% Generated by Mochi compiler v0.10.28 on 2025-07-18T03:06:15Z
:- style_check(-singleton).
print_val(V) :- number(V), !, format('~g', [V]).
print_val(V) :- write(V).

:- initialization(main, main).
main :-
    Xs = [1, 2, 3],
    (member(2, Xs) -> _V0 = true ; _V0 = false),
    print_val(_V0), nl,
    (member(5, Xs) -> _V1 = true ; _V1 = false),
    (_V1 -> _V2 = false ; _V2 = true),
    print_val(_V2), nl,
    true.
