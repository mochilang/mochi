:- style_check(-singleton).
:- initialization(main, main).
main :-
    (("a" @< "b") -> _V0 = true ; _V0 = false),
    writeln(_V0),
    (("a" @=< "a") -> _V1 = true ; _V1 = false),
    writeln(_V1),
    (("b" @> "a") -> _V2 = true ; _V2 = false),
    writeln(_V2),
    (("b" @>= "b") -> _V3 = true ; _V3 = false),
    writeln(_V3),
    true.
