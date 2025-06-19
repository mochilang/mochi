:- style_check(-singleton).
slice(Str, I, J, Out) :-
    string(Str), !,
    Len is J - I,
    sub_string(Str, I, Len, _, Out).
slice(List, I, J, Out) :-
    length(Prefix, I),
    append(Prefix, Rest, List),
    Len is J - I,
    length(Out, Len),
    append(Out, _, Rest).


get_item(Container, Key, Val) :-
    is_dict(Container), !, get_dict(Key, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


		digit(Ch, Res) :-
			catch(
				(
		(Ch =:= "0" ->
			throw(return(0))
		;
		true
		),
		(Ch =:= "1" ->
			throw(return(1))
		;
		true
		),
		(Ch =:= "2" ->
			throw(return(2))
		;
		true
		),
		(Ch =:= "3" ->
			throw(return(3))
		;
		true
		),
		(Ch =:= "4" ->
			throw(return(4))
		;
		true
		),
		(Ch =:= "5" ->
			throw(return(5))
		;
		true
		),
		(Ch =:= "6" ->
			throw(return(6))
		;
		true
		),
		(Ch =:= "7" ->
			throw(return(7))
		;
		true
		),
		(Ch =:= "8" ->
			throw(return(8))
		;
		true
		),
		(Ch =:= "9" ->
			throw(return(9))
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
			digit(Ch, Res) :-
			_V1 is -(1),
			Res = _V1.

		myatoi(S, Res) :-
			catch(
				(
		nb_setval(myatoi_i, 0),
		length(S, _V2),
		N = _V2,
		catch(
			(
				repeat,
					nb_getval(myatoi_i, _V3),
					nb_getval(myatoi_i, _V4),
					get_item(S, _V4, _V5),
					get_item(" ", 0, _V6),
					((_V3 < N, _V5 =:= _V6) ->
						nb_getval(myatoi_i, _V7),
						_V8 is _V7 + 1,
						nb_setval(myatoi_i, _V8),
						fail
					; true)
			)
			, break, true),
		nb_setval(myatoi_sign, 1),
		nb_getval(myatoi_i, _V9),
		nb_getval(myatoi_i, _V10),
		get_item(S, _V10, _V11),
		get_item("+", 0, _V12),
		nb_getval(myatoi_i, _V13),
		get_item(S, _V13, _V14),
		get_item("-", 0, _V15),
		((_V9 < N, (_V11 =:= _V12 ; _V14 =:= _V15)) ->
			nb_getval(myatoi_i, _V16),
			get_item(S, _V16, _V17),
			get_item("-", 0, _V18),
			(_V17 =:= _V18 ->
				_V19 is -(1),
				nb_setval(myatoi_sign, _V19)
			;
			true
			),
			nb_getval(myatoi_i, _V20),
			_V21 is _V20 + 1,
			nb_setval(myatoi_i, _V21)
		;
		true
		),
		nb_setval(myatoi_result, 0),
		catch(
			(
				repeat,
					nb_getval(myatoi_i, _V22),
					(_V22 < N ->
						nb_getval(myatoi_i, _V23),
						nb_getval(myatoi_i, _V24),
						_V25 is _V24 + 1,
						slice(S, _V23, _V25, _V26),
						Ch = _V26,
						digit(Ch, _V27),
						D = _V27,
						(D < 0 ->
							throw(break)
						;
						true
						),
						nb_getval(myatoi_result, _V28),
						_V29 is _V28 * 10,
						_V30 is _V29 + D,
						nb_setval(myatoi_result, _V30),
						nb_getval(myatoi_i, _V31),
						_V32 is _V31 + 1,
						nb_setval(myatoi_i, _V32),
						fail
					; true)
			)
			, break, true),
		nb_getval(myatoi_result, _V33),
		nb_getval(myatoi_sign, _V34),
		_V35 is _V33 * _V34,
		nb_setval(myatoi_result, _V35),
		nb_getval(myatoi_result, _V36),
		(_V36 > 2147483647 ->
			throw(return(2147483647))
		;
		true
		),
		nb_getval(myatoi_result, _V37),
		_V38 is -(2147483648),
		(_V37 < _V38 ->
			_V39 is -(2147483648),
			throw(return(_V39))
		;
		true
		)
					,
					true
				)
				, return(_V40),
					Res = _V40
				)
			.
			myatoi(S, Res) :-
			nb_getval(myatoi_result, _V41),
			Res = _V41.

	main :- true.
:- initialization(main, main).
