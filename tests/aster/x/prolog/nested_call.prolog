:- initialization(main).
:- style_check(-singleton).
foo(X) :-
    bar(X).
bar(X) :-
    writeln(X).
main :-
    foo(hello).
