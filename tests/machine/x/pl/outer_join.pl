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
    findall(_V13, (((member(O, Orders), findall(C, (member(C, Customers), get_item(O, 'customerid', _V8), get_item(C, 'id', _V9), (_V8 == _V9)), _V10), (_V10 = [] -> C = nil; member(C, _V10)));(member(C, Customers), findall([O], (member(O, Orders), get_item(O, 'customerid', _V8), get_item(C, 'id', _V9), (_V8 == _V9)), _V11), _V11 = [], O = nil)), true, dict_create(_V12, map, [order-O, customer-C]), _V13 = _V12), _V14),
    Result = _V14,
    write("--- Outer Join using syntax ---"),
    nl,
    catch(
        (
            member(Row, Result),
                catch(
                    (
                        get_item(Row, 'order', _V15),
                        (_V15 \= nil ->
                            get_item(Row, 'customer', _V16),
                            (_V16 \= nil ->
                                write("Order"),
                                write(' '),
                                get_item(Row, 'order', _V17),
                                get_item(_V17, 'id', _V18),
                                write(_V18),
                                write(' '),
                                write("by"),
                                write(' '),
                                get_item(Row, 'customer', _V19),
                                get_item(_V19, 'name', _V20),
                                write(_V20),
                                write(' '),
                                write("- $"),
                                write(' '),
                                get_item(Row, 'order', _V21),
                                get_item(_V21, 'total', _V22),
                                write(_V22),
                                nl,
                                true
                            ;
                                write("Order"),
                                write(' '),
                                get_item(Row, 'order', _V23),
                                get_item(_V23, 'id', _V24),
                                write(_V24),
                                write(' '),
                                write("by"),
                                write(' '),
                                write("Unknown"),
                                write(' '),
                                write("- $"),
                                write(' '),
                                get_item(Row, 'order', _V25),
                                get_item(_V25, 'total', _V26),
                                write(_V26),
                                nl,
                                true
                            ),
                            true
                        ;
                            write("Customer"),
                            write(' '),
                            get_item(Row, 'customer', _V27),
                            get_item(_V27, 'name', _V28),
                            write(_V28),
                            write(' '),
                            write("has no orders"),
                            nl,
                            true
                        ),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
