:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


        main :-
    Nums = [1, 2, 3],
    Letters = ["A", "B"],
    to_list(Nums, _V2),
    findall(N, (member(N, _V2), _V3 is N mod 2, _V3 = 0), _V4),
    to_list(Letters, _V5),
    findall(_V6, (member(N, _V4), member(L, _V5), dict_create(_V0, map, [n-N, l-L]), _V6 = _V0), _V7),
    Pairs = _V7,
    write("--- Even pairs ---"),
    nl,
    to_list(Pairs, _V8),
    catch(
        (
            member(P, _V8),
            catch(
                (
                    get_dict(n, P, _V9),
                    write(_V9),
                    write(' '),
                    get_dict(l, P, _V10),
                    write(_V10),
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
