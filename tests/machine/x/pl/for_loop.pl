:- style_check(-singleton).
:- initialization(main, main).
main :-
    catch(
        (
            _V0 is 4 - 1,
            between(1, _V0, I),
                catch(
                    (
                        write(I),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
