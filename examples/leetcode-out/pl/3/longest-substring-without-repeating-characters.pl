:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, get_dict(Key, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


		lengthoflongestsubstring(S, Res) :-
			catch(
				(
		length(S, _V0),
		N = _V0,
		nb_setval(lengthoflongestsubstring_start, 0),
		nb_setval(lengthoflongestsubstring_best, 0),
		nb_setval(lengthoflongestsubstring_i, 0),
		catch(
			(
				repeat,
					nb_getval(lengthoflongestsubstring_i, _V1),
					(_V1 < N ->
						nb_getval(lengthoflongestsubstring_start, _V2),
						nb_setval(lengthoflongestsubstring_j, _V2),
						catch(
							(
								repeat,
									nb_getval(lengthoflongestsubstring_j, _V3),
									nb_getval(lengthoflongestsubstring_i, _V4),
									(_V3 < _V4 ->
										nb_getval(lengthoflongestsubstring_j, _V5),
										get_item(S, _V5, _V6),
										nb_getval(lengthoflongestsubstring_i, _V7),
										get_item(S, _V7, _V8),
										(_V6 =:= _V8 ->
											nb_getval(lengthoflongestsubstring_j, _V9),
											_V10 is _V9 + 1,
											nb_setval(lengthoflongestsubstring_start, _V10),
											throw(break)
										;
										true
										),
										nb_getval(lengthoflongestsubstring_j, _V11),
										_V12 is _V11 + 1,
										nb_setval(lengthoflongestsubstring_j, _V12),
										fail
									; true)
							)
							, break, true),
						nb_getval(lengthoflongestsubstring_i, _V13),
						nb_getval(lengthoflongestsubstring_start, _V14),
						_V15 is _V13 - _V14,
						_V16 is _V15 + 1,
						Length = _V16,
						nb_getval(lengthoflongestsubstring_best, _V17),
						(Length > _V17 ->
							nb_setval(lengthoflongestsubstring_best, Length)
						;
						true
						),
						nb_getval(lengthoflongestsubstring_i, _V18),
						_V19 is _V18 + 1,
						nb_setval(lengthoflongestsubstring_i, _V19),
						fail
					; true)
			)
			, break, true)
					,
					true
				)
				, return(_V20),
					Res = _V20
				)
			.
			lengthoflongestsubstring(S, Res) :-
			nb_getval(lengthoflongestsubstring_best, _V21),
			Res = _V21.

	main :- true.
:- initialization(main, main).
