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
    Orders = [_V3, _V4, _V5],
    findall(_V11, (member(O, Orders), member(C, Customers), true, get_item(O, 'id', _V6), get_item(O, 'customerId', _V7), get_item(C, 'name', _V8), get_item(O, 'total', _V9), dict_create(_V10, map, [orderid-_V6, ordercustomerid-_V7, pairedcustomername-_V8, ordertotal-_V9]), _V11 = _V10), _V12),
    Result = _V12,
    write("--- Cross Join: All order-customer pairs ---"),
    nl,
    catch(
        (
            member(Entry, Result),
                catch(
                    (
                        write("Order"),
                        write(' '),
                        get_item(Entry, 'orderId', _V13),
                        write(_V13),
                        write(' '),
                        write("(customerId:"),
                        write(' '),
                        get_item(Entry, 'orderCustomerId', _V14),
                        write(_V14),
                        write(' '),
                        write(", total: $"),
                        write(' '),
                        get_item(Entry, 'orderTotal', _V15),
                        write(_V15),
                        write(' '),
                        write(") paired with"),
                        write(' '),
                        get_item(Entry, 'pairedCustomerName', _V16),
                        write(_V16),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
