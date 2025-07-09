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
    dict_create(_V2, map, [Id-3, Name-"Charlie"]),
    Customers = [_V0, _V1, _V2],
    dict_create(_V3, map, [Id-100, CustomerId-1, Total-250]),
    dict_create(_V4, map, [Id-101, CustomerId-2, Total-125]),
    dict_create(_V5, map, [Id-102, CustomerId-1, Total-300]),
    Orders = [_V3, _V4, _V5],
    get_item(O, 'id', _V6),
    get_item(O, 'customerId', _V7),
    get_item(C, 'name', _V8),
    get_item(O, 'total', _V9),
    dict_create(_V10, map, [OrderId-_V6, OrderCustomerId-_V7, PairedCustomerName-_V8, OrderTotal-_V9]),
    findall(_V10, (member(O, Orders), member(C, Customers), true), _V11),
    Result = _V11,
    writeln("--- Cross Join: All order-customer pairs ---"),
    catch(
        (
            member(Entry, Result),
                catch(
                    (
                        writeln("Order"),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
