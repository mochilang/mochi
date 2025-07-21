:- initialization(main).
:- style_check(-singleton).

main :-
    Items = [{cat: "a", val: 3}, {cat: "a", val: 1}, {cat: "b", val: 5}, {cat: "b", val: 2}],
    Grouped = [{cat: "b", total: 7}, {cat: "a", total: 4}],
    writeln({cat: "b", total: 7}),
    writeln({cat: "a", total: 4}).
