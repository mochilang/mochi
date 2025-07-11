:- style_check(-singleton).
:- initialization(main, main).
main :-
    (("a" @< "b") -> _V0 = true ; _V0 = false),
    write(_V0),
    nl,
    (("a" @=< "a") -> _V1 = true ; _V1 = false),
    write(_V1),
    nl,
    (("b" @> "a") -> _V2 = true ; _V2 = false),
    write(_V2),
    nl,
    (("b" @>= "b") -> _V3 = true ; _V3 = false),
    write(_V3),
    nl,
    true.
