% Generated by Mochi compiler v0.10.26 on 2025-07-15T06:34:32Z
:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

contains(Container, Item, Res) :-
    is_dict(Container), !, (string(Item) -> atom_string(A, Item) ; A = Item), (get_dict(A, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, (sub_string(List, _, _, _, Item) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).

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

avg(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), avg_list(Items, R).
avg(V, R) :-
    is_list(V), !, avg_list(V, R).
avg(_, _) :- throw(error('avg expects list or group')).
avg_list([], 0).
avg_list(L, R) :- sum_list(L, S), length(L, N), N > 0, R is S / N.

sum(V, R) :-
    is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).
sum(V, R) :-
    is_list(V), !, sum_list(V, R).
sum(_, _) :- throw(error('sum expects list or group')).

group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).
group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).
group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).
group_pairs([], Acc, Res) :- reverse(Acc, Res).
group_pairs([K-V|T], Acc, Res) :- group_insert(K, V, Acc, Acc1), group_pairs(T, Acc1, Res).
group_by(List, Fn, Groups) :- findall(K-V, (member(V, List), call(Fn, V, K)), Pairs), group_pairs(Pairs, [], Groups).

:- use_module(library(http/json)).
load_data(Path, Opts, Rows) :-
    (is_dict(Opts), get_dict(format, Opts, Fmt) -> true ; Fmt = 'json'),
    (Path == '' ; Path == '-' -> read_string(user_input, _, Text) ; read_file_to_string(Path, Text, [])),
    (Fmt == 'jsonl' ->
        split_string(Text, '\n', ' \t\r', Lines0),
        exclude(=(''), Lines0, Lines),
        findall(D, (member(L, Lines), open_string(L, S), json_read_dict(S, D), close(S)), Rows)
    ;
        open_string(Text, S), json_read_dict(S, Data), close(S),
        (is_list(Data) -> Rows = Data ; Rows = [Data])
    ).

expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [cs_bill_customer_sk-1, cs_sales_price-600, cs_sold_date_sk-1]),
    Catalog_sales = [_V0],
    dict_create(_V1, map, [c_customer_sk-1, c_current_addr_sk-1]),
    Customer = [_V1],
    dict_create(_V2, map, [ca_address_sk-1, ca_zip-"85669", ca_state-"CA"]),
    Customer_address = [_V2],
    dict_create(_V3, map, [d_date_sk-1, d_qoy-1, d_year-2000]),
    Date_dim = [_V3],
    findall(_V22, (member(Cs, Catalog_sales), member(C, Customer), get_item(Cs, 'cs_bill_customer_sk', _V4), get_item(C, 'c_customer_sk', _V5), (_V4 == _V5), member(Ca, Customer_address), get_item(C, 'c_current_addr_sk', _V6), get_item(Ca, 'ca_address_sk', _V7), (_V6 == _V7), member(D, Date_dim), get_item(Cs, 'cs_sold_date_sk', _V8), get_item(D, 'd_date_sk', _V9), (_V8 == _V9), get_item(Ca, 'ca_zip', _V10), substr(_V10, 0, 5, _V11), get_item(Ca, 'ca_state', _V12), get_item(Cs, 'cs_sales_price', _V13), contains(["85669", "86197", "88274", "83405", "86475", "85392", "85460", "80348", "81792"], _V11, _V14), contains(["CA", "WA", "GA"], _V12, _V15), get_item(D, 'd_qoy', _V16), get_item(D, 'd_year', _V17), ((((_V14 ; _V15) ; (_V13 > 500)), (_V16 == 1)), (_V17 == 2000)), get_item(Ca, 'ca_zip', _V18), dict_create(_V19, map, [zip-_V18]), _V20 = _V19, dict_create(_V21, map, ['Cs'-Cs, 'C'-C, 'Ca'-Ca, 'D'-D]), _V22 = _V20-_V21), _V23),
    group_pairs(_V23, [], _V24),
    findall(_V32, (member(G, _V24), get_item(G, 'key', _V25), get_item(_V25, 'zip', _V26), findall(_V28, (member(X, G), true, get_item(X, 'cs_sales_price', _V27), _V28 = _V27), _V29), sum(_V29, _V30), dict_create(_V31, map, [ca_zip-_V26, sum_sales-_V30]), _V32 = _V31), _V33),
    Filtered = _V33,
    json_write_dict(current_output, Filtered), nl,
    true,
    dict_create(_V34, map, [ca_zip-"85669", sum_sales-600]),
    expect((Filtered == [_V34])),
    true.
