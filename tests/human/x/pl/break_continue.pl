:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


        main :-
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    to_list(Numbers, _V0),
    catch(
        (
            member(N, _V0),
            catch(
                (
                    _V1 is N mod 2,
                    (_V1 = 0 ->
                        throw(continue)
                    ;
                    true
                    ),
                    (N > 7 ->
                        throw(break)
                    ;
                    true
                    ),
                    write("odd number:"),
                    write(' '),
                    write(N),
                    nl,
                    true
                ), continue, true),
                fail
                ;
                true
            )
            , break, true),
            true
        .
:- initialization(main, main).
