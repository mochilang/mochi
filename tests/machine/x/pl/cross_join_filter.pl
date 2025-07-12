:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).

:- initialization(main, main).
main :-
    Nums = [1, 2, 3],
    Letters = ["A", "B"],
    findall(_V1, (member(N, Nums), member(L, Letters), ((N mod 2) =:= 0), dict_create(_V0, map, [n-N, l-L]), _V1 = _V0), _V2),
    Pairs = _V2,
    writeln("--- Even pairs ---"),
    catch(
        (
            member(P, Pairs),
                catch(
                    (
                        get_item(P, 'n', _V3),
                        write(_V3),
                        write(' '),
                        get_item(P, 'l', _V4),
                        write(_V4),
                        nl,
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
