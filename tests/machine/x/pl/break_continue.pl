:- style_check(-singleton).
main :-
    Numbers is [1, 2, 3, 4, 5, 6, 7, 8, 9],
    (member(N, Numbers),
        (((N mod 2) =:= 0) ->
            % unsupported: unsupported statement
            true.
        :- initialization(main, main).
