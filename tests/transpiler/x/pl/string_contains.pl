:- style_check(-singleton).
:- initialization(main).

main :-
    S = 'catch',
    (sub_string(S, _, _, _, 'cat') -> writeln(true) ; writeln(false)),
    (sub_string(S, _, _, _, 'dog') -> writeln(true) ; writeln(false)).
