:- style_check(-singleton).

:- initialization main.

main :-
    (true, true, true->writeln(true);writeln(false)), (true, false, true->writeln(true);writeln(false)), (true, true, false, true->writeln(true);writeln(false)).
