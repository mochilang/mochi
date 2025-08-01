% Generated by Mochi compiler v0.10.26 on 2025-07-15T06:34:31Z
:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

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
    dict_create(_V0, map, [ss_store_sk-1, ss_sold_date_sk-1, ss_hdemo_sk-1, ss_cdemo_sk-1, ss_addr_sk-1, ss_sales_price-120, ss_net_profit-150, ss_quantity-10, ss_ext_sales_price-100, ss_ext_wholesale_cost-50]),
    Store_sales = [_V0],
    dict_create(_V1, map, [s_store_sk-1, s_state-"CA"]),
    Store = [_V1],
    dict_create(_V2, map, [cd_demo_sk-1, cd_marital_status-"M1", cd_education_status-"ES1"]),
    Customer_demographics = [_V2],
    dict_create(_V3, map, [hd_demo_sk-1, hd_dep_count-3]),
    Household_demographics = [_V3],
    dict_create(_V4, map, [ca_address_sk-1, ca_country-"United States", ca_state-"CA"]),
    Customer_address = [_V4],
    dict_create(_V5, map, [d_date_sk-1, d_year-2001]),
    Date_dim = [_V5],
    findall(_V22, (member(Ss, Store_sales), member(S, Store), get_item(Ss, 'ss_store_sk', _V6), get_item(S, 's_store_sk', _V7), (_V6 == _V7), member(Cd, Customer_demographics), get_item(Ss, 'ss_cdemo_sk', _V8), get_item(Cd, 'cd_demo_sk', _V9), get_item(Cd, 'cd_marital_status', _V10), get_item(Cd, 'cd_education_status', _V11), (((_V8 == _V9), (_V10 == "M1")), (_V11 == "ES1")), member(Hd, Household_demographics), get_item(Ss, 'ss_hdemo_sk', _V12), get_item(Hd, 'hd_demo_sk', _V13), get_item(Hd, 'hd_dep_count', _V14), ((_V12 == _V13), (_V14 == 3)), member(Ca, Customer_address), get_item(Ss, 'ss_addr_sk', _V15), get_item(Ca, 'ca_address_sk', _V16), get_item(Ca, 'ca_country', _V17), get_item(Ca, 'ca_state', _V18), (((_V15 == _V16), (_V17 == "United States")), (_V18 == "CA")), member(D, Date_dim), get_item(Ss, 'ss_sold_date_sk', _V19), get_item(D, 'd_date_sk', _V20), get_item(D, 'd_year', _V21), ((_V19 == _V20), (_V21 == 2001)), true, _V22 = Ss), _V23),
    Filtered = _V23,
    findall(_V27, (member(R, Filtered), true, dict_create(_V24, map, []), _V25 = _V24, dict_create(_V26, map, ['R'-R]), _V27 = _V25-_V26), _V28),
    group_pairs(_V28, [], _V29),
    findall(_V47, (member(G, _V29), findall(_V31, (member(X, G), true, get_item(X, 'ss_quantity', _V30), _V31 = _V30), _V32), avg(_V32, _V33), findall(_V35, (member(X, G), true, get_item(X, 'ss_ext_sales_price', _V34), _V35 = _V34), _V36), avg(_V36, _V37), findall(_V39, (member(X, G), true, get_item(X, 'ss_ext_wholesale_cost', _V38), _V39 = _V38), _V40), avg(_V40, _V41), findall(_V43, (member(X, G), true, get_item(X, 'ss_ext_wholesale_cost', _V42), _V43 = _V42), _V44), sum(_V44, _V45), dict_create(_V46, map, [avg_ss_quantity-_V33, avg_ss_ext_sales_price-_V37, avg_ss_ext_wholesale_cost-_V41, sum_ss_ext_wholesale_cost-_V45]), _V47 = _V46), _V48),
    Result = _V48,
    json_write_dict(current_output, Result), nl,
    true,
    dict_create(_V49, map, [avg_ss_quantity-10, avg_ss_ext_sales_price-100, avg_ss_ext_wholesale_cost-50, sum_ss_ext_wholesale_cost-50]),
    expect((Result == [_V49])),
    true.
