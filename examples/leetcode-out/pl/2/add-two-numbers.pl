:- style_check(-singleton).
		addtwonumbers(L1, L2, Res) :-
			catch(
				(
		nb_setval(addtwonumbers_i, 0),
		nb_setval(addtwonumbers_j, 0),
		nb_setval(addtwonumbers_carry, 0),
		nb_setval(addtwonumbers_result, []),
		catch(
			(
				repeat,
					nb_getval(addtwonumbers_i, _V0),
					length(L1, _V1),
					nb_getval(addtwonumbers_j, _V2),
					length(L2, _V3),
					nb_getval(addtwonumbers_carry, _V4),
					(((_V0 < _V1 ; _V2) < _V3 ; _V4) > 0 ->
						nb_setval(addtwonumbers_x, 0),
						nb_getval(addtwonumbers_i, _V5),
						length(L1, _V6),
						(_V5 < _V6 ->
							nb_getval(addtwonumbers_i, _V7),
							nth0(_V7, L1, _V8),
							nb_setval(addtwonumbers_x, _V8),
							nb_getval(addtwonumbers_i, _V9),
							_V10 is _V9 + 1,
							nb_setval(addtwonumbers_i, _V10)
						;
						true
						),
						nb_setval(addtwonumbers_y, 0),
						nb_getval(addtwonumbers_j, _V11),
						length(L2, _V12),
						(_V11 < _V12 ->
							nb_getval(addtwonumbers_j, _V13),
							nth0(_V13, L2, _V14),
							nb_setval(addtwonumbers_y, _V14),
							nb_getval(addtwonumbers_j, _V15),
							_V16 is _V15 + 1,
							nb_setval(addtwonumbers_j, _V16)
						;
						true
						),
						nb_getval(addtwonumbers_x, _V17),
						nb_getval(addtwonumbers_y, _V18),
						_V19 is _V17 + _V18,
						nb_getval(addtwonumbers_carry, _V20),
						_V21 is _V19 + _V20,
						Sum = _V21,
						_V22 is Sum mod 10,
						Digit = _V22,
						_V23 is Sum // 10,
						nb_setval(addtwonumbers_carry, _V23),
						nb_getval(addtwonumbers_result, _V24),
						append(_V24, [Digit], _V25),
						nb_setval(addtwonumbers_result, _V25),
						fail
					; true)
			)
			, break, true)
					,
					true
				)
				, return(_V26),
					Res = _V26
				)
			.
			addtwonumbers(L1, L2, Res) :-
			nb_getval(addtwonumbers_result, _V27),
			Res = _V27.

	main :- true.
:- initialization(main, main).
