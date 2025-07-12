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

len_any(Value, Len) :-
    string(Value), !, string_length(Value, Len).
len_any(Value, Len) :-
    is_dict(Value), !, dict_pairs(Value, _, Pairs), length(Pairs, Len).
len_any(Value, Len) :- length(Value, Len).

:- initialization(main, main).
main :-
    Prefix = "fore",
    S1 = "forest",
    len_any(Prefix, _V0),
    slice(S1, 0, _V0, _V1),
    ((_V1 == Prefix) -> _V2 = true ; _V2 = false),
    writeln(_V2),
    S2 = "desert",
    len_any(Prefix, _V3),
    slice(S2, 0, _V3, _V4),
    ((_V4 == Prefix) -> _V5 = true ; _V5 = false),
    writeln(_V5),
    true.
