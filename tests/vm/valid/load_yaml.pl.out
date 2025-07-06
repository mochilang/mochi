:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


:- use_module(library(http/json)).
load_data(Path, Opts, Rows) :-
    (is_dict(Opts), get_dict(format, Opts, Fmt) -> true ; Fmt = 'json'),
    (Path == '' ; Path == '-' -> read_string(user_input, _, Text) ; read_file_to_string(Path, Text, [])),
    (Fmt == 'jsonl' ->
        split_string(Text, '\n', ' \t\r', Lines0),
        exclude(=(""), Lines0, Lines),
        findall(D, (member(L, Lines), open_string(L, S), json_read_dict(S, D), close(S)), Rows)
    ;
        open_string(Text, S), json_read_dict(S, Data), close(S),
        (is_list(Data) -> Rows = Data ; Rows = [Data])
    ).



        main :-
    dict_create(_V0, map, [format-"yaml"]),
    load_data("../interpreter/valid/people.yaml", _V0, _V1),
    People = _V1,
    to_list(People, _V6),
    findall(P, (member(P, _V6), get_dict(age, P, _V7), _V7 >= 18), _V8),
    findall(_V9, (member(P, _V8), get_dict(name, P, _V2), get_dict(email, P, _V3), dict_create(_V4, map, [name-_V2, email-_V3]), _V9 = _V4), _V10),
    Adults = _V10,
    to_list(Adults, _V11),
    catch(
        (
            member(A, _V11),
            catch(
                (
                    get_dict(name, A, _V12),
                    write(_V12),
                    write(' '),
                    get_dict(email, A, _V13),
                    write(_V13),
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
