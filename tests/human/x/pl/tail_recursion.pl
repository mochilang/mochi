:- style_check(-singleton).
        sum_rec(N, Acc, Res) :-
            catch(
                (
        (N = 0 ->
            throw(return(Acc))
        ;
        true
        )
                    ,
                    true
                )
                , return(_V0),
                    Res = _V0
                )
            .
            sum_rec(N, Acc, Res) :-
            _V1 is N - 1,
            _V2 is Acc + N,
            sum_rec(_V1, _V2, _V3),
            Res = _V3.

    main :-
    sum_rec(10, 0, _V4),
    write(_V4),
    nl
    .
:- initialization(main, main).
