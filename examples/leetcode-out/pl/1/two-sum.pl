:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, get_dict(Key, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


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
				get_item(Nums, I, _V4),
				get_item(Nums, J, _V5),
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
	get_item(Result, 0, _V11),
	writeln(_V11),
	get_item(Result, 1, _V12),
	writeln(_V12)
	.
:- initialization(main, main).
