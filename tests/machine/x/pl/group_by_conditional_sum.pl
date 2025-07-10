:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [cat-"a", val-10, flag-true]),
    dict_create(_V1, map, [cat-"a", val-5, flag-false]),
    dict_create(_V2, map, [cat-"b", val-20, flag-true]),
    Items = [_V0, _V1, _V2],
    findall(_V14, (member(I, Items), true, get_item(G, 'key', _V3), findall(_V6, (member(X, G), true, get_item(X, 'flag', _V4), get_item(X, 'val', _V5), _V6 = (_V4 -> _V5 ; 0)), _V7), sum_list(_V7, _V8), findall(_V10, (member(X, G), true, get_item(X, 'val', _V9), _V10 = _V9), _V11), sum_list(_V11, _V12), dict_create(_V13, map, [cat-_V3, share-(_V8 / _V12)]), _V14 = _V13), _V15),
    Result = _V15,
    write(Result),
    nl,
    true.
