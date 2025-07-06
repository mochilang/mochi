:- style_check(-singleton).


    main :-
    dict_create(_V0, p_person, [name-"Bob", age-42]),
    dict_create(_V1, p_book, [title-"Go", author-_V0]),
    Book = _V1,
    get_dict(author, Book, _V2),
    get_dict(name, _V2, _V3),
    write(_V3),
    nl
    .
:- initialization(main, main).
