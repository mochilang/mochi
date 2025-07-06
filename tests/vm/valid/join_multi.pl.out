:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


        main :-
    dict_create(_V0, map, [id-1, name-"Alice"]),
    dict_create(_V1, map, [id-2, name-"Bob"]),
    Customers = [_V0, _V1],
    dict_create(_V2, map, [id-100, customerid-1]),
    dict_create(_V3, map, [id-101, customerid-2]),
    Orders = [_V2, _V3],
    dict_create(_V4, map, [orderid-100, sku-"a"]),
    dict_create(_V5, map, [orderid-101, sku-"b"]),
    Items = [_V4, _V5],
    to_list(Orders, _V9),
    to_list(Customers, _V12),
    to_list(Items, _V15),
    findall(_V16, (member(O, _V9), member(C, _V12), get_dict(customerid, O, _V10), get_dict(id, C, _V11), _V10 = _V11, member(I, _V15), get_dict(id, O, _V13), get_dict(orderid, I, _V14), _V13 = _V14, get_dict(name, C, _V6), get_dict(sku, I, _V7), dict_create(_V8, map, [name-_V6, sku-_V7]), _V16 = _V8), _V17),
    Result = _V17,
    write("--- Multi Join ---"),
    nl,
    to_list(Result, _V18),
    catch(
        (
            member(R, _V18),
            catch(
                (
                    get_dict(name, R, _V19),
                    write(_V19),
                    write(' '),
                    write("bought item"),
                    write(' '),
                    get_dict(sku, R, _V20),
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
