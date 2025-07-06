:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


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
    to_list(Orders, _V11),
    to_list(Customers, _V14),
    findall(_V15, (member(O, _V11), member(C, _V14), get_dict(customerid, O, _V12), get_dict(id, C, _V13), _V12 = _V13, get_dict(id, O, _V7), get_dict(name, C, _V8), get_dict(total, O, _V9), dict_create(_V10, map, [orderid-_V7, customername-_V8, total-_V9]), _V15 = _V10), _V16),
    Result = _V16,
    write("--- Orders with customer info ---"),
    nl,
    to_list(Result, _V17),
    catch(
        (
            member(Entry, _V17),
            catch(
                (
                    write("Order"),
                    write(' '),
                    get_dict(orderid, Entry, _V18),
                    write(_V18),
                    write(' '),
                    write("by"),
                    write(' '),
                    get_dict(customername, Entry, _V19),
                    write(_V19),
                    write(' '),
                    write("- $"),
                    write(' '),
                    get_dict(total, Entry, _V20),
                    write(_V20),
                    nl,
                    true
                ), continue, true),
                fail
                ;
                true
            )
            , break, true),
            true
        .
:- initialization(main, main).
