:- style_check(-singleton).
		twosum(Nums, Target, Res) :-
			catch(
				(
		length(Nums, _V0),
		N = _V0,
		_V1 is N - 1,
		forall(between(0, _V1, I), (
			_V2 is I + 1,
			_V3 is N - 1,
			forall(between(_V2, _V3, J), (
				nth0(I, Nums, _V4),
				nth0(J, Nums, _V5),
				_V6 is _V4 + _V5,
				(_V6 =:= Target ->
					throw(return([I, J]))
				;
				true
				),
				true
			)),
			true
		))
					,
					true
				)
				, return(_V7),
					Res = _V7
				)
			.
			twosum(Nums, Target, Res) :-
			_V8 is -(1),
			_V9 is -(1),
			Res = [_V8, _V9].

	main :-
	twosum([2, 7, 11, 15], 9, _V10),
	Result = _V10,
	nth0(0, Result, _V11),
	writeln(_V11),
	nth0(1, Result, _V12),
	writeln(_V12)
	.
:- initialization(main, main).
