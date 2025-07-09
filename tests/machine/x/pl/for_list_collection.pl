:- style_check(-singleton).
:- initialization(main, main).
main :-
    catch(
        (
            member(N, [1, 2, 3]),
                catch(
                    (
                        writeln(N),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
