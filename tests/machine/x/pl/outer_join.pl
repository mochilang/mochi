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
    dict_create(_V3, map, [Id-4, Name-"Diana"]),
    Customers = [_V0, _V1, _V2, _V3],
    dict_create(_V4, map, [Id-100, CustomerId-1, Total-250]),
    dict_create(_V5, map, [Id-101, CustomerId-2, Total-125]),
    dict_create(_V6, map, [Id-102, CustomerId-1, Total-300]),
    dict_create(_V7, map, [Id-103, CustomerId-5, Total-80]),
    Orders = [_V4, _V5, _V6, _V7],
    get_item(O, 'customerId', _V8),
    get_item(C, 'id', _V9),
    dict_create(_V10, map, [Order-O, Customer-C]),
    findall(_V10, (member(O, Orders), member(C, Customers), (_V8 == _V9), true), _V11),
    Result = _V11,
    writeln("--- Outer Join using syntax ---"),
    catch(
        (
            member(Row, Result),
                catch(
                    (
                        get_item(Row, 'order', _V12),
                        (_V12 ->
                            get_item(Row, 'customer', _V13),
                            (_V13 ->
                                writeln("Order"),
                            ;
                                writeln("Order"),
                            ),
                        ;
                            writeln("Customer"),
                        ),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
