:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, get_dict(Key, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


		ispalindrome(X, Res) :-
			catch(
				(
		(X < 0 ->
			throw(return(false))
		;
		true
		),
		str(X, _V0),
		S = _V0,
		length(S, _V1),
		N = _V1,
		_V2 is N // 2,
		_V3 is _V2 - 1,
		forall(between(0, _V3, I), (
			get_item(S, I, _V4),
			_V5 is N - 1,
			_V6 is _V5 - I,
			get_item(S, _V6, _V7),
			(_V4 =\= _V7 ->
				throw(return(false))
			;
			true
			),
			true
		))
					,
					true
				)
				, return(_V8),
					Res = _V8
				)
			.
			ispalindrome(X, Res) :-
			Res = true.

	main :- true.
:- initialization(main, main).
