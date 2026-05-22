:- initialization(main).
:- style_check(-singleton).
:- use_module(library(lists)).

main :-
    sum_list([1,2,3,4], S),
    writeln(S).
