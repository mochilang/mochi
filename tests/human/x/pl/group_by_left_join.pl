:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


count(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), length(Items, R).
count(V, R) :-
    string(V), !, string_chars(V, C), length(C, R).
count(V, R) :-
    is_list(V), !, length(V, R).
count(_, _) :- throw(error('count expects list or group')).


group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).
group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).
group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).
group_pairs([], Acc, Res) :- reverse(Acc, Res).
group_pairs([K-V|T], Acc, Res) :- group_insert(K, V, Acc, Acc1), group_pairs(T, Acc1, Res).
group_by(List, Fn, Groups) :- findall(K-V, (member(V, List), call(Fn, V, K)), Pairs), group_pairs(Pairs, [], Groups).


        main :-
    dict_create(_V0, map, [id-1, name-"Alice"]),
    dict_create(_V1, map, [id-2, name-"Bob"]),
    dict_create(_V2, map, [id-3, name-"Charlie"]),
    Customers = [_V0, _V1, _V2],
    dict_create(_V3, map, [id-100, customerid-1]),
    dict_create(_V4, map, [id-101, customerid-1]),
    dict_create(_V5, map, [id-102, customerid-2]),
    Orders = [_V3, _V4, _V5],
    to_list(Customers, _V7),
    to_list(Orders, _V10),
    findall(_V12, (member(C, _V7), member(O, _V10), get_dict(customerid, O, _V8), get_dict(id, C, _V9), _V8 = _V9, get_dict(name, C, _V6), _V11 = _V6, _V12 = _V11-C), _V13),
    group_pairs(_V13, [], _V14),
    findall(_V25, (member(G, _V14), get_dict(key, G, _V15), get_dict('Items', G, _V16), to_list(_V16, _V18), findall(R, (member(R, _V18), get_dict(o, R, _V19), _V19), _V20), findall(_V21, (member(R, _V20), _V21 = R), _V22), count(_V22, _V23), dict_create(_V24, map, [name-_V15, count-_V23]), _V25 = _V24), _V26),
    Stats = _V26,
    write("--- Group Left Join ---"),
    nl,
    to_list(Stats, _V27),
    catch(
        (
            member(S, _V27),
            catch(
                (
                    get_dict(name, S, _V28),
                    write(_V28),
                    write(' '),
                    write("orders:"),
                    write(' '),
                    get_dict(count, S, _V29),
                    write(_V29),
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
