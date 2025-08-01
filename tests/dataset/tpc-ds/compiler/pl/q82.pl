% Generated by Mochi compiler v0.10.26 on 2025-07-15T06:39:07Z
:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

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
    dict_create(_V0, map, [id-1]),
    dict_create(_V1, map, [id-2]),
    dict_create(_V2, map, [id-3]),
    Item = [_V0, _V1, _V2],
    dict_create(_V3, map, [item-1, qty-20]),
    dict_create(_V4, map, [item-1, qty-22]),
    dict_create(_V5, map, [item-1, qty-5]),
    dict_create(_V6, map, [item-2, qty-30]),
    dict_create(_V7, map, [item-2, qty-5]),
    dict_create(_V8, map, [item-3, qty-10]),
    Inventory = [_V3, _V4, _V5, _V6, _V7, _V8],
    dict_create(_V9, map, [item-1]),
    dict_create(_V10, map, [item-2]),
    Store_sales = [_V9, _V10],
    _V11 is 0,
    nb_setval(result, _V11),
    catch(
        (
            member(Inv, Inventory),
                catch(
                    (
                        catch(
                            (
                                member(S, Store_sales),
                                    catch(
                                        (
                                            get_item(Inv, 'item', _V12),
                                            get_item(S, 'item', _V13),
                                            ((_V12 == _V13) ->
                                                nb_getval(result, _V14),
                                                get_item(Inv, 'qty', _V15),
                                                _V16 is (_V14 + _V15),
                                                nb_setval(result, _V16),
                                                true
                                            ; true
                                            ),
                                            true
                                        ), continue, true),
                                        fail
                                    ; true
                                ), break, true),
                                true
                            ), continue, true),
                            fail
                        ; true
                    ), break, true),
                    nb_getval(result, _V17),
                    json_write_dict(current_output, _V17), nl,
                    true,
                    nb_getval(result, _V18),
                    expect((_V18 == 82)),
                    true.
