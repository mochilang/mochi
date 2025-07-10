:- style_check(-singleton).
:- initialization(main, main).
main :-
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    catch(
        (
            member(N, Numbers),
                catch(
                    (
                        (((N mod 2) == 0) ->
                            throw(continue),
                        ; true
                        ),
                        ((N > 7) ->
                            throw(break),
                        ; true
                        ),
                        write("odd number:"),
                        write(' '),
                        write(N),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
