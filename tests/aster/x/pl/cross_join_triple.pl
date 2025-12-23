:- initialization(main).
:- style_check(-singleton).
main :-
        Nums = [1, 2],
            Letters = ['A', 'B'],
            Bools = [true, false],
            Combos = [_{b: true, l: 'A', n: 1}, _{b: false, l: 'A', n: 1}, _{b: true, l: 'B', n: 1}, _{n: 1, b: false, l: 'B'}, _{b: true, l: 'A', n: 2}, _{b: false, l: 'A', n: 2}, _{b: true, l: 'B', n: 2}, _{b: false, l: 'B', n: 2}],
            writeln('--- Cross Join of three lists ---'),
            C = _{l: 'A', n: 1, b: true},
            writeln('1 A true'),
            C1 = _{l: 'A', n: 1, b: false},
            writeln('1 A false'),
            C2 = _{b: true, l: 'B', n: 1},
            writeln('1 B true'),
            C3 = _{b: false, l: 'B', n: 1},
            writeln('1 B false'),
            C4 = _{b: true, l: 'A', n: 2},
            writeln('2 A true'),
            C5 = _{l: 'A', n: 2, b: false},
            writeln('2 A false'),
            C6 = _{b: true, l: 'B', n: 2},
            writeln('2 B true'),
            C7 = _{b: false, l: 'B', n: 2},
        writeln('2 B false').
