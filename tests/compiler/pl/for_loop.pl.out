:- style_check(-singleton).
        main :-
    _V0 is 4 - 1,
    catch(
        (
            between(1, _V0, I),
            catch(
                (
                    write(I),
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
