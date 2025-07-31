:- initialization main.

:- style_check(-singleton).

main :-
    _11684 is 7, _11690 is 4, writeln(7), (_11684=:=7->writeln(true);writeln(false)), (_11690<5->writeln(true);writeln(false)).
