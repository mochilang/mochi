:- style_check(-singleton).
slice(Str, I, J, Out) :-
    string(Str), !,
    Len is J - I,
    sub_string(Str, I, Len, _, Out).
slice(List, I, J, Out) :-
    length(Prefix, I),
    append(Prefix, Rest, List),
    Len is J - I,
    length(Out, Len),
    append(Out, _, Rest).


    main :-
    Prefix = "fore",
    S1 = "forest",
    length(Prefix, _V0),
    slice(S1, 0, _V0, _V1),
    write(_V1 == Prefix),
    nl,
    S2 = "desert",
    length(Prefix, _V2),
    slice(S2, 0, _V2, _V3),
    write(_V3 == Prefix),
    nl
    .
:- initialization(main, main).
