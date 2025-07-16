:- style_check(-singleton).
        boom(Res) :-
            catch(
                (
        write("boom"),
        nl
                    ,
                    true
                )
                , return(_V0),
                    Res = _V0
                )
            .
            boom(Res) :-
            Res = true.

    main :-
    write(((1 < 2, 2 < 3), 3 < 4)),
    nl,
    boom(_V1),
    write(((1 < 2, 2 > 3), _V1)),
    nl,
    boom(_V2),
    write((((1 < 2, 2 < 3), 3 > 4), _V2)),
    nl
    .
:- initialization(main, main).
