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
    Orders = [_V3, _V4, _V5],
    to_list(Orders, _V11),
    to_list(Customers, _V12),
    findall(_V13, (member(O, _V11), member(C, _V12), get_dict(id, O, _V6), get_dict(customerid, O, _V7), get_dict(name, C, _V8), get_dict(total, O, _V9), dict_create(_V10, map, [orderid-_V6, ordercustomerid-_V7, pairedcustomername-_V8, ordertotal-_V9]), _V13 = _V10), _V14),
    Result = _V14,
    write("--- Cross Join: All order-customer pairs ---"),
    nl,
    to_list(Result, _V15),
    catch(
        (
            member(Entry, _V15),
            catch(
                (
                    write("Order"),
                    write(' '),
                    get_dict(orderid, Entry, _V16),
                    write(_V16),
                    write(' '),
                    write("(customerId:"),
                    write(' '),
                    get_dict(ordercustomerid, Entry, _V17),
                    write(_V17),
                    write(' '),
                    write(", total: $"),
                    write(' '),
                    get_dict(ordertotal, Entry, _V18),
                    write(_V18),
                    write(' '),
                    write(") paired with"),
                    write(' '),
                    get_dict(pairedcustomername, Entry, _V19),
                    write(_V19),
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
