:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, get_dict(Key, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


contains(Container, Item, Res) :-
    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).


		myatoi(S, Res) :-
			catch(
				(
		nb_setval(myatoi_i, 0),
		length(S, _V0),
		N = _V0,
		catch(
			(
				repeat,
					nb_getval(myatoi_i, _V1),
					nb_getval(myatoi_i, _V2),
					get_item(S, _V2, _V3),
					((_V1 < N, _V3) =:= " " ->
						nb_getval(myatoi_i, _V4),
						_V5 is _V4 + 1,
						nb_setval(myatoi_i, _V5),
						fail
					; true)
			)
			, break, true),
		nb_setval(myatoi_sign, 1),
		nb_getval(myatoi_i, _V6),
		nb_getval(myatoi_i, _V7),
		get_item(S, _V7, _V8),
		nb_getval(myatoi_i, _V9),
		get_item(S, _V9, _V10),
		((_V6 < N, (_V8 =:= "+" ; _V10) =:= "-") ->
			nb_getval(myatoi_i, _V11),
			get_item(S, _V11, _V12),
			(_V12 =:= "-" ->
				_V13 is -(1),
				nb_setval(myatoi_sign, _V13)
			;
			true
			),
			nb_getval(myatoi_i, _V14),
			_V15 is _V14 + 1,
			nb_setval(myatoi_i, _V15)
		;
		true
		),
		Digits = _{"0":0, "1":1, "2":2, "3":3, "4":4, "5":5, "6":6, "7":7, "8":8, "9":9},
		nb_setval(myatoi_result, 0),
		catch(
			(
				repeat,
					nb_getval(myatoi_i, _V16),
					(_V16 < N ->
						nb_getval(myatoi_i, _V17),
						get_item(S, _V17, _V18),
						Ch = _V18,
						contains(Digits, Ch, _V19),
						(\+ _V19 -> _V20 = true ; _V20 = false),
						(_V20 ->
							throw(break)
						;
						true
						),
						get_item(Digits, Ch, _V21),
						D = _V21,
						nb_getval(myatoi_result, _V22),
						_V23 is _V22 * 10,
						_V24 is _V23 + D,
						nb_setval(myatoi_result, _V24),
						nb_getval(myatoi_i, _V25),
						_V26 is _V25 + 1,
						nb_setval(myatoi_i, _V26),
						fail
					; true)
			)
			, break, true),
		nb_getval(myatoi_result, _V27),
		nb_getval(myatoi_sign, _V28),
		_V29 is _V27 * _V28,
		nb_setval(myatoi_result, _V29),
		nb_getval(myatoi_result, _V30),
		(_V30 > 2147483647 ->
			throw(return(2147483647))
		;
		true
		),
		nb_getval(myatoi_result, _V31),
		_V32 is -(2147483648),
		(_V31 < _V32 ->
			_V33 is -(2147483648),
			throw(return(_V33))
		;
		true
		)
					,
					true
				)
				, return(_V34),
					Res = _V34
				)
			.
			myatoi(S, Res) :-
			nb_getval(myatoi_result, _V35),
			Res = _V35.

	main :- true.
:- initialization(main, main).
