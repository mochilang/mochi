:- initialization(main).
:- style_check(-singleton).

main :-
    Book = _{tag: "Book", title: "Go", author: _{tag: "Person", name: "Bob", age: 42}},
    get_dict(name, get_dict(author, Book, R), V1), writeln(V1).
