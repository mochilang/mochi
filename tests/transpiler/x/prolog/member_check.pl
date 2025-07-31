:- initialization(main).
:- style_check(-singleton).

main :-
    (member(3, [1,2,3]) -> writeln(found) ; writeln(missing)).
