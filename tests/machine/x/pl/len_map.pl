:- style_check(-singleton).
len_any(Value, Len) :-
    string(Value), !, string_length(Value, Len).
len_any(Value, Len) :-
    is_dict(Value), !, dict_pairs(Value, _, Pairs), length(Pairs, Len).
len_any(Value, Len) :- length(Value, Len).

:- initialization(main, main).
main :-
    dict_create(_V0, map, ['a'-1, 'b'-2]),
    len_any(_V0, _V1),
    _V2 is _V1,
    writeln(_V2),
    true.
