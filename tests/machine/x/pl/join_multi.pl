:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [Id-1, Name-"Alice"]),
    dict_create(_V1, map, [Id-2, Name-"Bob"]),
    Customers = [_V0, _V1],
    dict_create(_V2, map, [Id-100, CustomerId-1]),
    dict_create(_V3, map, [Id-101, CustomerId-2]),
    Orders = [_V2, _V3],
    dict_create(_V4, map, [OrderId-100, Sku-"a"]),
    dict_create(_V5, map, [OrderId-101, Sku-"b"]),
    Items = [_V4, _V5],
    get_item(O, 'customerId', _V6),
    get_item(C, 'id', _V7),
    get_item(O, 'id', _V8),
    get_item(I, 'orderId', _V9),
    get_item(C, 'name', _V10),
    get_item(I, 'sku', _V11),
    dict_create(_V12, map, [Name-_V10, Sku-_V11]),
    findall(_V12, (member(O, Orders), member(C, Customers), (_V6 == _V7), member(I, Items), (_V8 == _V9), true), _V13),
    Result = _V13,
    writeln("--- Multi Join ---"),
    catch(
        (
            member(R, Result),
                catch(
                    (
                        get_item(R, 'name', _V14),
                        writeln(_V14),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
