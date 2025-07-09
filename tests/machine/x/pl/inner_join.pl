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
    Customers = [_V0, _V1, _V2],
    dict_create(_V3, map, [id-100, customerid-1, total-250]),
    dict_create(_V4, map, [id-101, customerid-2, total-125]),
    dict_create(_V5, map, [id-102, customerid-1, total-300]),
    dict_create(_V6, map, [id-103, customerid-4, total-80]),
    Orders = [_V3, _V4, _V5, _V6],
    findall(_V13, (member(O, Orders), member(C, Customers), get_item(O, 'customerId', _V7), get_item(C, 'id', _V8), (_V7 == _V8), true, get_item(O, 'id', _V9), get_item(C, 'name', _V10), get_item(O, 'total', _V11), dict_create(_V12, map, [orderid-_V9, customername-_V10, total-_V11]), _V13 = _V12), _V14),
    Result = _V14,
    write("--- Orders with customer info ---"),
    nl,
    catch(
        (
            member(Entry, Result),
                catch(
                    (
                        write("Order"),
                        write(' '),
                        get_item(Entry, 'orderId', _V15),
                        write(_V15),
                        write(' '),
                        write("by"),
                        write(' '),
                        get_item(Entry, 'customerName', _V16),
                        write(_V16),
                        write(' '),
                        write("- $"),
                        write(' '),
                        get_item(Entry, 'total', _V17),
                        write(_V17),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
