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
    dict_create(_V2, map, [id-100, customerid-1]),
    dict_create(_V3, map, [id-101, customerid-2]),
    Orders = [_V2, _V3],
    dict_create(_V4, map, [orderid-100, sku-"a"]),
    Items = [_V4],
    findall(_V13, (member(O, Orders), member(C, Customers), get_item(O, 'customerid', _V5), get_item(C, 'id', _V6), (_V5 == _V6), findall(I, (member(I, Items), get_item(O, 'id', _V7), get_item(I, 'orderid', _V8), (_V7 == _V8)), _V9), (_V9 = [] -> I = nil; member(I, _V9)), true, get_item(O, 'id', _V10), get_item(C, 'name', _V11), dict_create(_V12, map, [orderid-_V10, name-_V11, item-I]), _V13 = _V12), _V14),
    Result = _V14,
    write("--- Left Join Multi ---"),
    nl,
    catch(
        (
            member(R, Result),
                catch(
                    (
                        get_item(R, 'orderid', _V15),
                        write(_V15),
                        write(' '),
                        get_item(R, 'name', _V16),
                        write(_V16),
                        write(' '),
                        get_item(R, 'item', _V17),
                        write(_V17),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
