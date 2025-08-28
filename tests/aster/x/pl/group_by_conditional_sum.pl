:- initialization(main).
:- style_check(-singleton).
main :-
        Items = [_{cat: a, val: 10, flag: true}, _{cat: a, val: 5, flag: false}, _{cat: b, val: 20, flag: true}],
            Result = [_{cat: a, share: 0.6666666666666666}, _{cat: b, share: 1}],
            writeln(_{cat: a, share: 0.6666666666666666}),
        writeln(_{cat: b, share: 1}).
