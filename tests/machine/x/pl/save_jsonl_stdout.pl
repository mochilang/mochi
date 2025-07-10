:- style_check(-singleton).
:- use_module(library(http/json)).
save_data(Rows, Path, Opts) :-
    (is_dict(Opts), get_dict(format, Opts, Fmt) -> true ; Fmt = 'json'),
    (Path == '' ; Path == '-' -> Out = current_output ; open(Path, write, Out)),
    (Fmt == 'jsonl' ->
        forall(member(R, Rows), (json_write_dict(Out, R), nl(Out)))
    ;
        json_write_dict(Out, Rows)
    ),
    (Out == current_output -> flush_output(Out) ; close(Out)).

:- initialization(main, main).
main :-
    dict_create(_V0, map, [name-"Alice", age-30]),
    dict_create(_V1, map, [name-"Bob", age-25]),
    People = [_V0, _V1],
    dict_create(_V2, map, [format-"jsonl"]),
    save_data(People, "-", _V2),
    true.
