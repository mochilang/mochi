:- style_check(-singleton).
set_item(Container, Key, Val, Out) :-
    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), put_dict(A, Container, Val, Out).
set_item(List, Index, Val, Out) :-
    nth0(Index, List, _, Rest),
    nth0(Index, Out, Val, Rest).

expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).

    people_update([], []).
    people_update([_V4|_V5], [_V6|_V7]) :-
        get_item(_V4, 'age', Age),
        get_item(_V4, 'status', Status),
        ((Age @>= 18) ->
            set_item(_V4, 'status', "adult", _V8),
            set_item(_V8, 'age', (Age + 1), _V9),
            _V6 = _V9
        ;
            _V6 = _V4
        ),
        people_update(_V5, _V7).
    
    test_update_adult_status :-
        dict_create(_V0, p_person, ['name'-"Alice", 'age'-17, 'status'-"minor"]),
        dict_create(_V1, p_person, ['name'-"Bob", 'age'-26, 'status'-"adult"]),
        dict_create(_V2, p_person, ['name'-"Charlie", 'age'-19, 'status'-"adult"]),
        dict_create(_V3, p_person, ['name'-"Diana", 'age'-16, 'status'-"minor"]),
        expect((People == [_V0, _V1, _V2, _V3])),
        true.
    
:- initialization(main, main).
main :-
    dict_create(_V0, p_person, ['name'-"Alice", 'age'-17, 'status'-"minor"]),
    dict_create(_V1, p_person, ['name'-"Bob", 'age'-25, 'status'-"unknown"]),
    dict_create(_V2, p_person, ['name'-"Charlie", 'age'-18, 'status'-"unknown"]),
    dict_create(_V3, p_person, ['name'-"Diana", 'age'-16, 'status'-"minor"]),
    People = [_V0, _V1, _V2, _V3],
    people_update(People, _V10),
    People_11 = _V10,
    test_update_adult_status,
    writeln("ok"),
    true.
