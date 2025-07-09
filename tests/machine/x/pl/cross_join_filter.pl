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
    dict_create(_V0, map, [N-N, L-L]),
    findall(_V0, (member(N, Nums), member(L, Letters), ((N mod 2) == 0)), _V1),
    Pairs = _V1,
    writeln("--- Even pairs ---"),
    catch(
        (
            member(P, Pairs),
                catch(
                    (
                        get_item(P, 'n', _V2),
                        writeln(_V2),
                        true
                    ), continue, true),
                    fail
                ; true
            ), break, true),
            true.
