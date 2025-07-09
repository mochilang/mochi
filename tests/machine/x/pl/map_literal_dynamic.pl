:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    X is 3,
    Y is 4,
    dict_create(_V0, map, ['a'-X, 'b'-Y]),
    M = _V0,
    get_item(M, "a", _V1),
    writeln(_V1),
    true.
