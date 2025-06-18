:- style_check(-singleton).
to_list(Str, L) :-
    string(Str), !,
    string_chars(Str, L).
to_list(L, L).


get_item(Container, Key, Val) :-
    is_dict(Container), !, get_dict(Key, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


set_item(Container, Key, Val, Out) :-
    is_dict(Container), !, put_dict(Key, Container, Val, Out).
set_item(List, Index, Val, Out) :-
    nth0(Index, List, _, Rest),
    nth0(Index, Out, Val, Rest).


		convert(S, NumRows, Res) :-
			catch(
				(
		length(S, _V0),
		((NumRows =< 1 ; NumRows) >= _V0 ->
			throw(return(S))
		;
		true
		),
		nb_setval(convert_rows, []),
		nb_setval(convert_i, 0),
		catch(
			(
				repeat,
					nb_getval(convert_i, _V1),
					(_V1 < NumRows ->
						nb_getval(convert_rows, _V2),
						append(_V2, [""], _V3),
						nb_setval(convert_rows, _V3),
						nb_getval(convert_i, _V4),
						_V5 is _V4 + 1,
						nb_setval(convert_i, _V5),
						fail
					; true)
			)
			, break, true),
		nb_setval(convert_curr, 0),
		nb_setval(convert_step, 1),
		to_list(S, _V6),
		forall(member(Ch, _V6), (
			nb_getval(convert_rows, _V12),
			nb_getval(convert_curr, _V7),
			nb_getval(convert_rows, _V8),
			nb_getval(convert_curr, _V9),
			get_item(_V8, _V9, _V10),
			_V11 is _V10 + Ch,
			set_item(_V12, _V7, _V11, _V13),
			nb_setval(convert_rows, _V13),
			nb_getval(convert_curr, _V14),
			(_V14 =:= 0 ->
				nb_setval(convert_step, 1)
			;
			nb_getval(convert_curr, _V15),
			_V16 is _V15 =:= NumRows - 1,
			(_V16 ->
				_V17 is -(1),
				nb_setval(convert_step, _V17)
			;
			true
			)
			),
			nb_getval(convert_curr, _V18),
			nb_getval(convert_step, _V19),
			_V20 is _V18 + _V19,
			nb_setval(convert_curr, _V20),
			true
		)),
		nb_setval(convert_result, ""),
		nb_getval(convert_rows, _V21),
		to_list(_V21, _V22),
		forall(member(Row, _V22), (
			nb_getval(convert_result, _V23),
			_V24 is _V23 + Row,
			nb_setval(convert_result, _V24),
			true
		))
					,
					true
				)
				, return(_V25),
					Res = _V25
				)
			.
			convert(S, NumRows, Res) :-
			nb_getval(convert_result, _V26),
			Res = _V26.

	main :- true.
:- initialization(main, main).
