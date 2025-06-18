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


contains(Container, Item, Res) :-
    is_dict(Container), !, (get_dict(Item, Container, _) -> Res = true ; Res = false).
contains(List, Item, Res) :-
    string(List), !, string_chars(List, Chars), (member(Item, Chars) -> Res = true ; Res = false).
contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).


		ismatch(S, P, Res) :-
			catch(
				(
		length(S, _V0),
		M = _V0,
		length(P, _V1),
		N = _V1,
		nb_setval(ismatch_memo, _{}),
				dfs(I, J, Res) :-
					catch(
						(
				_V2 is N + 1,
				_V3 is I * _V2,
				_V4 is _V3 + J,
				Key = _V4,
				nb_getval(ismatch_memo, _V5),
				contains(_V5, Key, _V6),
				(_V6 ->
					nb_getval(ismatch_memo, _V7),
					get_item(_V7, Key, _V8),
					throw(return(_V8))
				;
				true
				),
				(J =:= N ->
					throw(return(I =:= M))
				;
				true
				),
				nb_setval(dfs_first, false),
				(I < M ->
					get_item(P, J, _V9),
					get_item(S, I, _V10),
					get_item(P, J, _V11),
					((_V9 =:= _V10 ; _V11 =:= ".") ->
						nb_setval(dfs_first, true)
					;
					true
					)
				;
				true
				),
				nb_setval(dfs_ans, false),
				_V12 is J + 1,
				(_V12 < N ->
					_V13 is J + 1,
					get_item(P, _V13, _V14),
					(_V14 =:= "*" ->
						_V15 is J + 2,
						dfs(I, _V15, _V16),
						(_V16 ->
							nb_setval(dfs_ans, true)
						;
						nb_getval(dfs_first, _V17),
						_V18 is I + 1,
						dfs(_V18, J, _V19),
						((_V17, _V19) ->
							nb_setval(dfs_ans, true)
						;
						true
						)
						)
					;
						nb_getval(dfs_first, _V20),
						_V21 is I + 1,
						_V22 is J + 1,
						dfs(_V21, _V22, _V23),
						((_V20, _V23) ->
							nb_setval(dfs_ans, true)
						;
						true
						)
					)
				;
					nb_getval(dfs_first, _V24),
					_V25 is I + 1,
					_V26 is J + 1,
					dfs(_V25, _V26, _V27),
					((_V24, _V27) ->
						nb_setval(dfs_ans, true)
					;
					true
					)
				),
				nb_getval(ismatch_memo, _V29),
				nb_getval(dfs_ans, _V28),
				set_item(_V29, Key, _V28, _V30),
				nb_setval(ismatch_memo, _V30)
							,
							true
						)
						, return(_V31),
							Res = _V31
						)
					.
					dfs(I, J, Res) :-
					nb_getval(dfs_ans, _V32),
					Res = _V32.
					,
					true
				)
				, return(_V33),
					Res = _V33
				)
			.
			ismatch(S, P, Res) :-
			dfs(0, 0, _V34),
			Res = _V34.

	main :- true.
:- initialization(main, main).
