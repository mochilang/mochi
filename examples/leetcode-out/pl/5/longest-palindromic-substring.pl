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


		expand(S, Left, Right, Res) :-
			catch(
				(
		nb_setval(expand_l, Left),
		nb_setval(expand_r, Right),
		length(S, _V0),
		N = _V0,
		catch(
			(
				repeat,
					nb_getval(expand_l, _V1),
					nb_getval(expand_r, _V2),
					((_V1 >= 0, _V2 < N) ->
						nb_getval(expand_l, _V3),
						get_item(S, _V3, _V4),
						nb_getval(expand_r, _V5),
						get_item(S, _V5, _V6),
						(_V4 =\= _V6 ->
							throw(break)
						;
						true
						),
						nb_getval(expand_l, _V7),
						_V8 is _V7 - 1,
						nb_setval(expand_l, _V8),
						nb_getval(expand_r, _V9),
						_V10 is _V9 + 1,
						nb_setval(expand_r, _V10),
						fail
					; true)
			)
			, break, true)
					,
					true
				)
				, return(_V11),
					Res = _V11
				)
			.
			expand(S, Left, Right, Res) :-
			nb_getval(expand_r, _V12),
			nb_getval(expand_l, _V13),
			_V14 is _V12 - _V13,
			_V15 is _V14 - 1,
			Res = _V15.

		longestpalindrome(S, Res) :-
			catch(
				(
		length(S, _V16),
		(_V16 =< 1 ->
			throw(return(S))
		;
		true
		),
		nb_setval(longestpalindrome_start, 0),
		nb_setval(longestpalindrome_end, 0),
		length(S, _V17),
		N = _V17,
		_V18 is N - 1,
		forall(between(0, _V18, I), (
			expand(S, I, I, _V19),
			Len1 = _V19,
			_V20 is I + 1,
			expand(S, I, _V20, _V21),
			Len2 = _V21,
			nb_setval(longestpalindrome_l, Len1),
			(Len2 > Len1 ->
				nb_setval(longestpalindrome_l, Len2)
			;
			true
			),
			nb_getval(longestpalindrome_l, _V22),
			nb_getval(longestpalindrome_end, _V23),
			nb_getval(longestpalindrome_start, _V24),
			_V25 is _V23 - _V24,
			(_V22 > _V25 ->
				nb_getval(longestpalindrome_l, _V26),
				_V27 is _V26 - 1,
				_V28 is _V27 // 2,
				_V29 is I - _V28,
				nb_setval(longestpalindrome_start, _V29),
				nb_getval(longestpalindrome_l, _V30),
				_V31 is _V30 // 2,
				_V32 is I + _V31,
				nb_setval(longestpalindrome_end, _V32)
			;
			true
			),
			true
		))
					,
					true
				)
				, return(_V33),
					Res = _V33
				)
			.
			longestpalindrome(S, Res) :-
			nb_getval(longestpalindrome_start, _V34),
			nb_getval(longestpalindrome_end, _V35),
			_V36 is _V35 + 1,
			slice(S, _V34, _V36, _V37),
			Res = _V37.

	main :- true.
:- initialization(main, main).
