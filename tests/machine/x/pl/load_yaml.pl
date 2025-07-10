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

:- initialization(main, main).
main :-
    dict_create(_V0, map, [format-"yaml"]),
    load_data("../interpreter/valid/people.yaml", _V0, _V1),
    People = _V1,
    findall(_V6, (member(P, People), get_item(P, 'age', _V2), (_V2 >= 18), get_item(P, 'name', _V3), get_item(P, 'email', _V4), dict_create(_V5, map, [name-_V3, email-_V4]), _V6 = _V5), _V7),
    Adults = _V7,
    catch(
        (
            member(A, Adults),
                catch(
                    (
                        get_item(A, 'name', _V8),
                        write(_V8),
                        write(' '),
                        get_item(A, 'email', _V9),
                        write(_V9),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
