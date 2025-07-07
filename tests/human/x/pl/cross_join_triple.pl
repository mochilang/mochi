:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


        main :-
    Nums = [1, 2],
    Letters = ["A", "B"],
    Bools = [true, false],
    to_list(Nums, _V1),
    to_list(Letters, _V2),
    to_list(Bools, _V3),
    findall(_V4, (member(N, _V1), member(L, _V2), member(B, _V3), dict_create(_V0, map, [n-N, l-L, b-B]), _V4 = _V0), _V5),
    Combos = _V5,
    write("--- Cross Join of three lists ---"),
    nl,
    to_list(Combos, _V6),
    catch(
        (
            member(C, _V6),
            catch(
                (
                    get_dict(n, C, _V7),
                    write(_V7),
                    write(' '),
                    get_dict(l, C, _V8),
                    write(_V8),
                    write(' '),
                    get_dict(b, C, _V9),
                    write(_V9),
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
