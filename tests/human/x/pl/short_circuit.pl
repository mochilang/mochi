:- style_check(-singleton).
        boom(A, B, Res) :-
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
            boom(A, B, Res) :-
            Res = true.

    main :-
    boom(1, 2, _V1),
    write((false, _V1)),
    nl,
    boom(1, 2, _V2),
    write((true ; _V2)),
    nl
    .
:- initialization(main, main).
