:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, p_person, ['name'-"Bob", 'age'-42]),
    dict_create(_V1, p_book, ['title'-"Go", 'author'-_V0]),
    Book = _V1,
    get_item(Book, 'author', _V2),
    get_item(_V2, 'name', _V3),
    write(_V3),
    nl,
    true.
