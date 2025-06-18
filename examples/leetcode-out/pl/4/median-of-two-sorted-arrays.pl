:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, get_dict(Key, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


		findmediansortedarrays(Nums1, Nums2, Res) :-
			catch(
				(
		nb_setval(findmediansortedarrays_merged, []),
		nb_setval(findmediansortedarrays_i, 0),
		nb_setval(findmediansortedarrays_j, 0),
		catch(
			(
				repeat,
					nb_getval(findmediansortedarrays_i, _V0),
					length(Nums1, _V1),
					nb_getval(findmediansortedarrays_j, _V2),
					length(Nums2, _V3),
					((_V0 < _V1 ; _V2) < _V3 ->
						nb_getval(findmediansortedarrays_j, _V4),
						length(Nums2, _V5),
						(_V4 >= _V5 ->
							nb_getval(findmediansortedarrays_merged, _V6),
							nb_getval(findmediansortedarrays_i, _V7),
							get_item(Nums1, _V7, _V8),
							append(_V6, [_V8], _V9),
							nb_setval(findmediansortedarrays_merged, _V9),
							nb_getval(findmediansortedarrays_i, _V10),
							_V11 is _V10 + 1,
							nb_setval(findmediansortedarrays_i, _V11)
						;
						nb_getval(findmediansortedarrays_i, _V12),
						length(Nums1, _V13),
						(_V12 >= _V13 ->
							nb_getval(findmediansortedarrays_merged, _V14),
							nb_getval(findmediansortedarrays_j, _V15),
							get_item(Nums2, _V15, _V16),
							append(_V14, [_V16], _V17),
							nb_setval(findmediansortedarrays_merged, _V17),
							nb_getval(findmediansortedarrays_j, _V18),
							_V19 is _V18 + 1,
							nb_setval(findmediansortedarrays_j, _V19)
						;
						nb_getval(findmediansortedarrays_i, _V20),
						get_item(Nums1, _V20, _V21),
						nb_getval(findmediansortedarrays_j, _V22),
						get_item(Nums2, _V22, _V23),
						(_V21 =< _V23 ->
							nb_getval(findmediansortedarrays_merged, _V24),
							nb_getval(findmediansortedarrays_i, _V25),
							get_item(Nums1, _V25, _V26),
							append(_V24, [_V26], _V27),
							nb_setval(findmediansortedarrays_merged, _V27),
							nb_getval(findmediansortedarrays_i, _V28),
							_V29 is _V28 + 1,
							nb_setval(findmediansortedarrays_i, _V29)
						;
							nb_getval(findmediansortedarrays_merged, _V30),
							nb_getval(findmediansortedarrays_j, _V31),
							get_item(Nums2, _V31, _V32),
							append(_V30, [_V32], _V33),
							nb_setval(findmediansortedarrays_merged, _V33),
							nb_getval(findmediansortedarrays_j, _V34),
							_V35 is _V34 + 1,
							nb_setval(findmediansortedarrays_j, _V35)
						)
						)
						),
						fail
					; true)
			)
			, break, true),
		nb_getval(findmediansortedarrays_merged, _V36),
		length(_V36, _V37),
		Total = _V37,
		_V38 is Total mod 2,
		(_V38 =:= 1 ->
			nb_getval(findmediansortedarrays_merged, _V39),
			_V40 is Total // 2,
			get_item(_V39, _V40, _V41),
			throw(return(_V41))
		;
		true
		),
		nb_getval(findmediansortedarrays_merged, _V42),
		_V43 is Total // 2,
		_V44 is _V43 - 1,
		get_item(_V42, _V44, _V45),
		Mid1 = _V45,
		nb_getval(findmediansortedarrays_merged, _V46),
		_V47 is Total // 2,
		get_item(_V46, _V47, _V48),
		Mid2 = _V48
					,
					true
				)
				, return(_V49),
					Res = _V49
				)
			.
			findmediansortedarrays(Nums1, Nums2, Res) :-
			_V50 is Mid1 + Mid2,
			_V51 is _V50 // 2,
			Res = _V51.

	main :- true.
:- initialization(main, main).
