:- style_check(-singleton).
		reverse(X, Res) :-
			catch(
				(
		nb_setval(reverse_sign, 1),
		nb_setval(reverse_n, X),
		nb_getval(reverse_n, _V0),
		(_V0 < 0 ->
			_V1 is -(1),
			nb_setval(reverse_sign, _V1),
			nb_getval(reverse_n, _V2),
			_V3 is -(_V2),
			nb_setval(reverse_n, _V3)
		;
		true
		),
		nb_setval(reverse_rev, 0),
		catch(
			(
				repeat,
					nb_getval(reverse_n, _V4),
					(_V4 =\= 0 ->
						nb_getval(reverse_n, _V5),
						_V6 is _V5 mod 10,
						Digit = _V6,
						nb_getval(reverse_rev, _V7),
						_V8 is _V7 * 10,
						_V9 is _V8 + Digit,
						nb_setval(reverse_rev, _V9),
						nb_getval(reverse_n, _V10),
						_V11 is _V10 // 10,
						nb_setval(reverse_n, _V11),
						fail
					; true)
			)
			, break, true),
		nb_getval(reverse_rev, _V12),
		nb_getval(reverse_sign, _V13),
		_V14 is _V12 * _V13,
		nb_setval(reverse_rev, _V14),
		nb_getval(reverse_rev, _V15),
		_V16 is -(2147483647),
		_V17 is _V16 - 1,
		nb_getval(reverse_rev, _V18),
		((_V15 < _V17 ; _V18 > 2147483647) ->
			throw(return(0))
		;
		true
		)
					,
					true
				)
				, return(_V19),
					Res = _V19
				)
			.
			reverse(X, Res) :-
			nb_getval(reverse_rev, _V20),
			Res = _V20.

	main :- true.
:- initialization(main, main).
