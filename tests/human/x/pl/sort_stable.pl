:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


    main :-
    dict_create(_V0, map, [n-1, v-"a"]),
    dict_create(_V1, map, [n-1, v-"b"]),
    dict_create(_V2, map, [n-2, v-"c"]),
    Items = [_V0, _V1, _V2],
    to_list(Items, _V5),
    findall(_V7-_V6, (member(I, _V5), get_dict(v, I, _V3), get_dict(n, I, _V4), _V7 = _V4, _V6 = _V3), _V8),
    keysort(_V8, _V9),
    findall(V, member(_-V, _V9), _V10),
    Result = _V10,
    write(Result),
    nl
    .
:- initialization(main, main).
