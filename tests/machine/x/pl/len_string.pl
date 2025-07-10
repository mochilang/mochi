:- style_check(-singleton).
len_any(Value, Len) :-
    string(Value), !, string_length(Value, Len).
len_any(Value, Len) :-
    is_dict(Value), !, dict_pairs(Value, _, Pairs), length(Pairs, Len).
len_any(Value, Len) :- length(Value, Len).

:- initialization(main, main).
main :-
    len_any("mochi", _V0),
    _V1 is _V0,
    write(_V1),
    nl,
    true.
