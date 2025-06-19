:- style_check(-singleton).
get_item(Container, Key, Val) :-
    is_dict(Container), !, get_dict(Key, Container, Val).
get_item(Container, Index, Val) :-
    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).
get_item(List, Index, Val) :- nth0(Index, List, Val).


set_item(Container, Key, Val, Out) :-
    is_dict(Container), !, put_dict(Key, Container, Val, Out).
set_item(List, Index, Val, Out) :-
    nth0(Index, List, _, Rest),
    nth0(Index, Out, Val, Rest).


		ismatch(S, P, Res) :-
			catch(
				(
		length(S, _V0),
		M = _V0,
		length(P, _V1),
		N = _V1,
		nb_setval(ismatch_dp, []),
		nb_setval(ismatch_i, 0),
		catch(
			(
				repeat,
					nb_getval(ismatch_i, _V2),
					(_V2 =< M ->
						nb_setval(ismatch_row, []),
						nb_setval(ismatch_j, 0),
						catch(
							(
								repeat,
									nb_getval(ismatch_j, _V3),
									(_V3 =< N ->
										nb_getval(ismatch_row, _V4),
										append(_V4, [false], _V5),
										nb_setval(ismatch_row, _V5),
										nb_getval(ismatch_j, _V6),
										_V7 is _V6 + 1,
										nb_setval(ismatch_j, _V7),
										fail
									; true)
							)
							, break, true),
						nb_getval(ismatch_dp, _V8),
						nb_getval(ismatch_row, _V9),
						append(_V8, [_V9], _V10),
						nb_setval(ismatch_dp, _V10),
						nb_getval(ismatch_i, _V11),
						_V12 is _V11 + 1,
						nb_setval(ismatch_i, _V12),
						fail
					; true)
			)
			, break, true),
		nb_getval(ismatch_dp, _V13),
		get_item(_V13, M, _V14),
		set_item(_V14, N, true, _V15),
		set_item(_V13, M, _V15, _V16),
		nb_setval(ismatch_dp, _V16),
		nb_setval(ismatch_i2, M),
		catch(
			(
				repeat,
					nb_getval(ismatch_i2, _V17),
					(_V17 >= 0 ->
						_V18 is N - 1,
						nb_setval(ismatch_j2, _V18),
						catch(
							(
								repeat,
									nb_getval(ismatch_j2, _V19),
									(_V19 >= 0 ->
										nb_setval(ismatch_first, false),
										nb_getval(ismatch_i2, _V20),
										(_V20 < M ->
											nb_getval(ismatch_j2, _V21),
											get_item(P, _V21, _V22),
											nb_getval(ismatch_i2, _V23),
											get_item(S, _V23, _V24),
											nb_getval(ismatch_j2, _V25),
											get_item(P, _V25, _V26),
											((_V22 =:= _V24 ; _V26 =:= ".") ->
												nb_setval(ismatch_first, true)
											;
											true
											)
										;
										true
										),
										nb_getval(ismatch_j2, _V27),
										_V31 is _V27 + 1,
										nb_getval(ismatch_j2, _V28),
										_V29 is _V28 + 1,
										get_item(P, _V29, _V30),
										((_V31 < N, _V30 =:= "*") ->
											nb_getval(ismatch_dp, _V32),
											nb_getval(ismatch_i2, _V33),
											get_item(_V32, _V33, _V34),
											nb_getval(ismatch_j2, _V35),
											_V36 is _V35 + 2,
											get_item(_V34, _V36, _V37),
											nb_getval(ismatch_first, _V38),
											nb_getval(ismatch_dp, _V39),
											nb_getval(ismatch_i2, _V40),
											_V41 is _V40 + 1,
											get_item(_V39, _V41, _V42),
											nb_getval(ismatch_j2, _V43),
											get_item(_V42, _V43, _V44),
											((_V37 ; (_V38, _V44)) ->
												nb_getval(ismatch_dp, _V47),
												nb_getval(ismatch_i2, _V45),
												get_item(_V47, _V45, _V48),
												nb_getval(ismatch_j2, _V46),
												set_item(_V48, _V46, true, _V49),
												set_item(_V47, _V45, _V49, _V50),
												nb_setval(ismatch_dp, _V50)
											;
												nb_getval(ismatch_dp, _V53),
												nb_getval(ismatch_i2, _V51),
												get_item(_V53, _V51, _V54),
												nb_getval(ismatch_j2, _V52),
												set_item(_V54, _V52, false, _V55),
												set_item(_V53, _V51, _V55, _V56),
												nb_setval(ismatch_dp, _V56)
											)
										;
											nb_getval(ismatch_first, _V57),
											nb_getval(ismatch_dp, _V58),
											nb_getval(ismatch_i2, _V59),
											_V60 is _V59 + 1,
											get_item(_V58, _V60, _V61),
											nb_getval(ismatch_j2, _V62),
											_V63 is _V62 + 1,
											get_item(_V61, _V63, _V64),
											((_V57, _V64) ->
												nb_getval(ismatch_dp, _V67),
												nb_getval(ismatch_i2, _V65),
												get_item(_V67, _V65, _V68),
												nb_getval(ismatch_j2, _V66),
												set_item(_V68, _V66, true, _V69),
												set_item(_V67, _V65, _V69, _V70),
												nb_setval(ismatch_dp, _V70)
											;
												nb_getval(ismatch_dp, _V73),
												nb_getval(ismatch_i2, _V71),
												get_item(_V73, _V71, _V74),
												nb_getval(ismatch_j2, _V72),
												set_item(_V74, _V72, false, _V75),
												set_item(_V73, _V71, _V75, _V76),
												nb_setval(ismatch_dp, _V76)
											)
										),
										nb_getval(ismatch_j2, _V77),
										_V78 is _V77 - 1,
										nb_setval(ismatch_j2, _V78),
										fail
									; true)
							)
							, break, true),
						nb_getval(ismatch_i2, _V79),
						_V80 is _V79 - 1,
						nb_setval(ismatch_i2, _V80),
						fail
					; true)
			)
			, break, true)
					,
					true
				)
				, return(_V81),
					Res = _V81
				)
			.
			ismatch(S, P, Res) :-
			nb_getval(ismatch_dp, _V82),
			get_item(_V82, 0, _V83),
			get_item(_V83, 0, _V84),
			Res = _V84.

	main :- true.
:- initialization(main, main).
