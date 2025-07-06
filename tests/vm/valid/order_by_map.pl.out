:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


    main :-
    dict_create(_V0, map, [a-1, b-2]),
    dict_create(_V1, map, [a-1, b-1]),
    dict_create(_V2, map, [a-0, b-5]),
    Data = [_V0, _V1, _V2],
    to_list(Data, _V6),
    findall(_V8-_V7, (member(X, _V6), get_dict(a, X, _V3), get_dict(b, X, _V4), dict_create(_V5, map, [a-_V3, b-_V4]), _V8 = _V5, _V7 = X), _V9),
    keysort(_V9, _V10),
    findall(V, member(_-V, _V10), _V11),
    Sorted = _V11,
    write(Sorted),
    nl
    .
:- initialization(main, main).
