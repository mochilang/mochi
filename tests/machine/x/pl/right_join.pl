:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [id-1, name-"Alice"]),
    dict_create(_V1, map, [id-2, name-"Bob"]),
    dict_create(_V2, map, [id-3, name-"Charlie"]),
    dict_create(_V3, map, [id-4, name-"Diana"]),
    Customers = [_V0, _V1, _V2, _V3],
    dict_create(_V4, map, [id-100, customerid-1, total-250]),
    dict_create(_V5, map, [id-101, customerid-2, total-125]),
    dict_create(_V6, map, [id-102, customerid-1, total-300]),
    Orders = [_V4, _V5, _V6],
    findall(_V12, (member(O, Orders), findall([C], (member(C, Customers), get_item(O, 'customerid', _V7), get_item(C, 'id', _V8), (_V7 == _V8)), _V9), (_V9 = [] -> (C = nil) ; member([C], _V9)), true, get_item(C, 'name', _V10), dict_create(_V11, map, [customername-_V10, order-O]), _V12 = _V11), _V13),
    Result = _V13,
    write("--- Right Join using syntax ---"),
    nl,
    catch(
        (
            member(Entry, Result),
                catch(
                    (
                        get_item(Entry, 'order', _V14),
                        (_V14 \= nil ->
                            write("Customer"),
                            write(' '),
                            get_item(Entry, 'customername', _V15),
                            write(_V15),
                            write(' '),
                            write("has order"),
                            write(' '),
                            get_item(Entry, 'order', _V16),
                            get_item(_V16, 'id', _V17),
                            write(_V17),
                            write(' '),
                            write("- $"),
                            write(' '),
                            get_item(Entry, 'order', _V18),
                            get_item(_V18, 'total', _V19),
                            write(_V19),
                            nl,
                        ;
                            write("Customer"),
                            write(' '),
                            get_item(Entry, 'customername', _V20),
                            write(_V20),
                            write(' '),
                            write("has no orders"),
                            nl,
                        ),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
