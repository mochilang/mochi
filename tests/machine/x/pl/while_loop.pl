:- style_check(-singleton).
:- initialization(main, main).
main :-
    I is 0,
    catch(
        (
            repeat,
            ((I < 3) ->
                catch(
                    (
                        write(I),
                        nl,
                        I_0 is (I_0 + 1),
                        true
                    ), continue, true),
                    fail
                ; true)
            ), break, true),
        true.
