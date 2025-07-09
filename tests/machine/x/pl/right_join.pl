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
    findall(_V11, (member(C, Customers), member(O, Orders), get_item(O, 'customerId', _V7), get_item(C, 'id', _V8), (_V7 == _V8), true, get_item(C, 'name', _V9), dict_create(_V10, map, [customername-_V9, order-O]), _V11 = _V10), _V12),
    Result = _V12,
    write("--- Right Join using syntax ---"),
    nl,
    catch(
        (
            member(Entry, Result),
                catch(
                    (
                        get_item(Entry, 'order', _V13),
                        (_V13 \= nil ->
                            write("Customer"),
                            write(' '),
                            get_item(Entry, 'customerName', _V14),
                            write(_V14),
                            write(' '),
                            write("has order"),
                            write(' '),
                            get_item(Entry, 'order', _V15),
                            get_item(_V15, 'id', _V16),
                            write(_V16),
                            write(' '),
                            write("- $"),
                            write(' '),
                            get_item(Entry, 'order', _V17),
                            get_item(_V17, 'total', _V18),
                            write(_V18),
                            nl,
                        ;
                            write("Customer"),
                            write(' '),
                            get_item(Entry, 'customerName', _V19),
                            write(_V19),
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
