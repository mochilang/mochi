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
										nb_setval(ismatch_star, false),
										nb_getval(ismatch_j2, _V27),
										_V28 is _V27 + 1,
										(_V28 < N ->
											nb_getval(ismatch_j2, _V29),
											_V30 is _V29 + 1,
											get_item(P, _V30, _V31),
											(_V31 =:= "*" ->
												nb_setval(ismatch_star, true)
											;
											true
											)
										;
										true
										),
										nb_getval(ismatch_star, _V32),
										(_V32 ->
											nb_getval(ismatch_dp, _V33),
											nb_getval(ismatch_i2, _V34),
											get_item(_V33, _V34, _V35),
											nb_getval(ismatch_j2, _V36),
											_V37 is _V36 + 2,
											get_item(_V35, _V37, _V38),
											nb_getval(ismatch_first, _V39),
											nb_getval(ismatch_dp, _V40),
											nb_getval(ismatch_i2, _V41),
											_V42 is _V41 + 1,
											get_item(_V40, _V42, _V43),
											nb_getval(ismatch_j2, _V44),
											get_item(_V43, _V44, _V45),
											((_V38 ; (_V39, _V45)) ->
												nb_getval(ismatch_dp, _V48),
												nb_getval(ismatch_i2, _V46),
												get_item(_V48, _V46, _V49),
												nb_getval(ismatch_j2, _V47),
												set_item(_V49, _V47, true, _V50),
												set_item(_V48, _V46, _V50, _V51),
												nb_setval(ismatch_dp, _V51)
											;
												nb_getval(ismatch_dp, _V54),
												nb_getval(ismatch_i2, _V52),
												get_item(_V54, _V52, _V55),
												nb_getval(ismatch_j2, _V53),
												set_item(_V55, _V53, false, _V56),
												set_item(_V54, _V52, _V56, _V57),
												nb_setval(ismatch_dp, _V57)
											)
										;
											nb_getval(ismatch_first, _V58),
											nb_getval(ismatch_dp, _V59),
											nb_getval(ismatch_i2, _V60),
											_V61 is _V60 + 1,
											get_item(_V59, _V61, _V62),
											nb_getval(ismatch_j2, _V63),
											_V64 is _V63 + 1,
											get_item(_V62, _V64, _V65),
											((_V58, _V65) ->
												nb_getval(ismatch_dp, _V68),
												nb_getval(ismatch_i2, _V66),
												get_item(_V68, _V66, _V69),
												nb_getval(ismatch_j2, _V67),
												set_item(_V69, _V67, true, _V70),
												set_item(_V68, _V66, _V70, _V71),
												nb_setval(ismatch_dp, _V71)
											;
												nb_getval(ismatch_dp, _V74),
												nb_getval(ismatch_i2, _V72),
												get_item(_V74, _V72, _V75),
												nb_getval(ismatch_j2, _V73),
												set_item(_V75, _V73, false, _V76),
												set_item(_V74, _V72, _V76, _V77),
												nb_setval(ismatch_dp, _V77)
											)
										),
										nb_getval(ismatch_j2, _V78),
										_V79 is _V78 - 1,
										nb_setval(ismatch_j2, _V79),
										fail
									; true)
							)
							, break, true),
						nb_getval(ismatch_i2, _V80),
						_V81 is _V80 - 1,
						nb_setval(ismatch_i2, _V81),
						fail
					; true)
			)
			, break, true)
					,
					true
				)
				, return(_V82),
					Res = _V82
				)
			.
			ismatch(S, P, Res) :-
			nb_getval(ismatch_dp, _V83),
			get_item(_V83, 0, _V84),
			get_item(_V84, 0, _V85),
			Res = _V85.

	main :- true.
:- initialization(main, main).
