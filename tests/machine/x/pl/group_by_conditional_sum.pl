:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [Cat-"a", Val-10, Flag-true]),
    dict_create(_V1, map, [Cat-"a", Val-5, Flag-false]),
    dict_create(_V2, map, [Cat-"b", Val-20, Flag-true]),
    Items = [_V0, _V1, _V2],
    get_item(G, 'key', _V3),
    get_item(X, 'flag', _V4),
    get_item(X, 'val', _V5),
    findall((_V4 -> _V5 ; 0), (member(X, G), true), _V6),
    sum_list(_V6, _V7),
    get_item(X, 'val', _V8),
    findall(_V8, (member(X, G), true), _V9),
    sum_list(_V9, _V10),
    dict_create(_V11, map, [Cat-_V3, Share-(_V7 / _V10)]),
    findall(_V11, (member(I, Items), true), _V12),
    Result = _V12,
    writeln(Result),
    true.
