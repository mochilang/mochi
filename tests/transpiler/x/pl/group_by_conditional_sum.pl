:- style_check(-singleton).
:- initialization(main).

main :-
    Items = [{cat: "a", val: 10, flag: true}, {cat: "a", val: 5, flag: false}, {cat: "b", val: 20, flag: true}],
    Result = [{cat: "a", share: 10 / 15}, {cat: "b", share: 20 / 20}],
    writeln({cat: "a", share: 10 / 15}),
    writeln({cat: "b", share: 20 / 20}).
