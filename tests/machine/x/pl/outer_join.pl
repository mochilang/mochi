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
    dict_create(_V7, map, [id-103, customerid-5, total-80]),
    Orders = [_V4, _V5, _V6, _V7],
    findall(_V11, (member(O, Orders), member(C, Customers), get_item(O, 'customerId', _V8), get_item(C, 'id', _V9), (_V8 == _V9), true, dict_create(_V10, map, [order-O, customer-C]), _V11 = _V10), _V12),
    Result = _V12,
    write("--- Outer Join using syntax ---"),
    nl,
    catch(
        (
            member(Row, Result),
                catch(
                    (
                        get_item(Row, 'order', _V13),
                        (_V13 \= nil ->
                            get_item(Row, 'customer', _V14),
                            (_V14 \= nil ->
                                write("Order"),
                                write(' '),
                                get_item(Row, 'order', _V15),
                                get_item(_V15, 'id', _V16),
                                write(_V16),
                                write(' '),
                                write("by"),
                                write(' '),
                                get_item(Row, 'customer', _V17),
                                get_item(_V17, 'name', _V18),
                                write(_V18),
                                write(' '),
                                write("- $"),
                                write(' '),
                                get_item(Row, 'order', _V19),
                                get_item(_V19, 'total', _V20),
                                write(_V20),
                                nl,
                            ;
                                write("Order"),
                                write(' '),
                                get_item(Row, 'order', _V21),
                                get_item(_V21, 'id', _V22),
                                write(_V22),
                                write(' '),
                                write("by"),
                                write(' '),
                                write("Unknown"),
                                write(' '),
                                write("- $"),
                                write(' '),
                                get_item(Row, 'order', _V23),
                                get_item(_V23, 'total', _V24),
                                write(_V24),
                                nl,
                            ),
                        ;
                            write("Customer"),
                            write(' '),
                            get_item(Row, 'customer', _V25),
                            get_item(_V25, 'name', _V26),
                            write(_V26),
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
