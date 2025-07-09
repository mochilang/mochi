:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    Nums = [1, 2],
    Letters = ["A", "B"],
    Bools = [true, false],
    dict_create(_V0, map, [N-N, L-L, B-B]),
    findall(_V0, (member(N, Nums), member(L, Letters), member(B, Bools), true), _V1),
    Combos = _V1,
    writeln("--- Cross Join of three lists ---"),
    catch(
        (
            member(C, Combos),
                catch(
                    (
                        get_item(C, 'n', _V2),
                        writeln(_V2),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
