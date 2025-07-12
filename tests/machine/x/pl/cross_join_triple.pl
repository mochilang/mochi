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
    findall(_V1, (member(N, Nums), member(L, Letters), member(B, Bools), true, dict_create(_V0, map, [n-N, l-L, b-B]), _V1 = _V0), _V2),
    Combos = _V2,
    writeln("--- Cross Join of three lists ---"),
    catch(
        (
            member(C, Combos),
                catch(
                    (
                        get_item(C, 'n', _V3),
                        write(_V3),
                        write(' '),
                        get_item(C, 'l', _V4),
                        write(_V4),
                        write(' '),
                        get_item(C, 'b', _V5),
                        write(_V5),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
