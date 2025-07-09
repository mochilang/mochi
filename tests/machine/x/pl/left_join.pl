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
    Customers = [_V0, _V1],
    dict_create(_V2, map, [id-100, customerid-1, total-250]),
    dict_create(_V3, map, [id-101, customerid-3, total-80]),
    Orders = [_V2, _V3],
    findall(_V10, (member(O, Orders), findall(C, (member(C, Customers), get_item(O, 'customerid', _V4), get_item(C, 'id', _V5), (_V4 == _V5)), _V6), (_V6 = [] -> C = nil; member(C, _V6)), true, get_item(O, 'id', _V7), get_item(O, 'total', _V8), dict_create(_V9, map, [orderid-_V7, customer-C, total-_V8]), _V10 = _V9), _V11),
    Result = _V11,
    write("--- Left Join ---"),
    nl,
    catch(
        (
            member(Entry, Result),
                catch(
                    (
                        write("Order"),
                        write(' '),
                        get_item(Entry, 'orderid', _V12),
                        write(_V12),
                        write(' '),
                        write("customer"),
                        write(' '),
                        get_item(Entry, 'customer', _V13),
                        write(_V13),
                        write(' '),
                        write("total"),
                        write(' '),
                        get_item(Entry, 'total', _V14),
                        write(_V14),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
