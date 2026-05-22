:- initialization(main).
:- style_check(-singleton).

main :-
    Items = [{cat: "a", val: 10, flag: true}, {cat: "a", val: 5, flag: false}, {cat: "b", val: 20, flag: true}],
    Result = [{cat: "a", share: 0.6666666666666666}, {cat: "b", share: 1}],
    writeln({cat: "a", share: 0.6666666666666666}),
    writeln({cat: "b", share: 1}).
