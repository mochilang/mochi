:- initialization(main).
:- style_check(-singleton).
main :-
        Todo = _{title: hi},
            get_dict(title, Todo, V1),
        writeln(V1).
