# Errors

- append_builtin: type error: error[T002]: undefined variable: _11214
  --> :5:9

help:
  Check if the variable was declared in this scope.
- avg_builtin: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: avg(V, R) :-
  3:     is_dict(V), !, get_dict('Items', V, Items), avg_list(Items, R).
  4: avg(V, R) :-
  5:     is_list(V), !, avg_list(V, R).
  6: avg(_, _) :- throw(error('avg expects list or group')).
  7: avg_list([], 0).
  8: avg_list(L, R) :- sum_list(L, S), length(L, N), N > 0, R is S / N.
  9: 
 10: 
- basic_compare: parse error: parse error: 7:15: unexpected token "=" (expected ")")
- binary_precedence: ok
- bool_chain: parse error: parse error: 2:9: unexpected token "(" (expected ")")
- break_continue: parse error: parse error: 15:94: lexer: invalid input text ";true), (_16720>..."
- cast_string_to_int: ok
- cast_struct: type error: error[T002]: undefined variable: hi
  --> :2:29

help:
  Check if the variable was declared in this scope.
- closure: parse error: parse error: 2:10: unexpected token "is" (expected "!=" <ident>)
- count_builtin: parse error: parse error: 2:20: unexpected token "!" (expected LogicCond)
- cross_join: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- cross_join_filter: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- cross_join_triple: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- dataset_sort_take_limit: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- dataset_where_filter: compile error: unsupported expression
- exists_builtin: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- for_list_collection: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- for_loop: ok
- for_map_collection: parse error: parse error: 2:61: unexpected token "(" (expected ")")
- fun_call: parse error: parse error: 2:10: unexpected token "is" (expected "!=" <ident>)
- fun_expr_in_let: parse error: parse error: 2:10: unexpected token "is" (expected "!=" <ident>)
- fun_three_args: parse error: parse error: 2:10: unexpected token "is" (expected "!=" <ident>)
- group_by: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: to_list(Str, L) :-
  3:     string(Str), !,
  4:     string_chars(Str, L).
  5: to_list(L, L).
  6: 
  7: 
  8: count(V, R) :-
  9:     is_dict(V), !, get_dict('Items', V, Items), length(Items, R).
 10: count(V, R) :-
- group_by_conditional_sum: compile error: unsupported expression
- group_by_having: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: to_list(Str, L) :-
  3:     string(Str), !,
  4:     string_chars(Str, L).
  5: to_list(L, L).
  6: 
  7: 
  8: count(V, R) :-
  9:     is_dict(V), !, get_dict('Items', V, Items), length(Items, R).
 10: count(V, R) :-
- group_by_join: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: to_list(Str, L) :-
  3:     string(Str), !,
  4:     string_chars(Str, L).
  5: to_list(L, L).
  6: 
  7: 
  8: count(V, R) :-
  9:     is_dict(V), !, get_dict('Items', V, Items), length(Items, R).
 10: count(V, R) :-
- group_by_left_join: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: to_list(Str, L) :-
  3:     string(Str), !,
  4:     string_chars(Str, L).
  5: to_list(L, L).
  6: 
  7: 
  8: count(V, R) :-
  9:     is_dict(V), !, get_dict('Items', V, Items), length(Items, R).
 10: count(V, R) :-
- group_by_multi_join: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: to_list(Str, L) :-
  3:     string(Str), !,
  4:     string_chars(Str, L).
  5: to_list(L, L).
  6: 
  7: 
  8: sum(V, R) :-
  9:     is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).
 10: sum(V, R) :-
- group_by_multi_join_sort: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: to_list(Str, L) :-
  3:     string(Str), !,
  4:     string_chars(Str, L).
  5: to_list(L, L).
  6: 
  7: 
  8: sum(V, R) :-
  9:     is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).
 10: sum(V, R) :-
- group_by_sort: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: to_list(Str, L) :-
  3:     string(Str), !,
  4:     string_chars(Str, L).
  5: to_list(L, L).
  6: 
  7: 
  8: sum(V, R) :-
  9:     is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).
 10: sum(V, R) :-
- group_items_iteration: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: to_list(Str, L) :-
  3:     string(Str), !,
  4:     string_chars(Str, L).
  5: to_list(L, L).
  6: 
  7: 
  8: group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).
  9: group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).
 10: group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).
- if_else: ok
- if_then_else: compile error: unsupported expression
- if_then_else_nested: compile error: unsupported expression
- in_operator: parse error: parse error: 2:69: lexer: invalid input text ";_13726=false)\nr..."
- in_operator_extended: parse error: parse error: 5:69: lexer: invalid input text ";_17192=false)\nr..."
- inner_join: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- join_multi: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- json_builtin: parse error: parse error: 3:1: unexpected token "fun" (expected "!=" <ident>)
- left_join: compile error: unsupported join side
- left_join_multi: compile error: unsupported join side
- len_builtin: type error: error[T002]: undefined variable: _11108
  --> :3:9

help:
  Check if the variable was declared in this scope.
- len_map: ok
- len_string: type error: error[T002]: undefined variable: mochi
  --> :2:20

help:
  Check if the variable was declared in this scope.
- let_and_print: type error: error[T002]: undefined variable: _11170
  --> :4:16

help:
  Check if the variable was declared in this scope.
- list_assign: parse error: parse error: 2:67: lexer: invalid input text ";_15278=_15262),..."
- list_index: parse error: parse error: 2:67: lexer: invalid input text ";_13094=_13078),..."
- list_nested_assign: parse error: parse error: 2:67: lexer: invalid input text ";_15902=_15886),..."
- list_set_ops: json: cannot unmarshal array into Go struct field clause.clauses.params of type string
  1: :- style_check(-singleton).
  2: union(A, B, R) :- append(A, B, C), list_to_set(C, R).
  3: 
  4: 
  5: except([], _, []).
  6: except([H|T], B, R) :- memberchk(H, B), !, except(T, B, R).
  7: except([H|T], B, [H|R]) :- except(T, B, R).
  8: 
  9: 
 10: intersect(A, B, R) :- intersect(A, B, [], R).
- load_yaml: parse error: parse error: 5:59: lexer: invalid input text ";_21120=json), (..."
- map_assign: parse error: parse error: 2:67: lexer: invalid input text ";_15596=_15580),..."
- map_in_operator: parse error: parse error: 2:69: lexer: invalid input text ";_13678=false)\nr..."
- map_index: parse error: parse error: 2:67: lexer: invalid input text ";_13298=_13282),..."
- map_int_key: parse error: parse error: 2:67: lexer: invalid input text ";_13286=_13270),..."
- map_literal_dynamic: parse error: parse error: 2:67: lexer: invalid input text ";_14564=_14548),..."
- map_membership: parse error: parse error: 2:69: lexer: invalid input text ";_13702=false)\nr..."
- map_nested_assign: parse error: parse error: 2:67: lexer: invalid input text ";_16448=_16432),..."
- match_expr: compile error: unsupported expression
- match_full: compile error: unsupported expression
- math_ops: type error: error[T002]: undefined variable: mod
  --> :6:18

help:
  Check if the variable was declared in this scope.
- membership: parse error: parse error: 2:69: lexer: invalid input text ";_13510=false)\nr..."
- min_max_builtin: parse error: parse error: 2:20: unexpected token "!" (expected LogicCond)
- nested_function: parse error: parse error: 4:29: unexpected token ")" (expected "(" (Expr ("," Expr)*)? ")")
- order_by_map: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- outer_join: compile error: unsupported join side
- partial_application: parse error: parse error: 2:10: unexpected token "is" (expected "!=" <ident>)
- print_hello: type error: error[T002]: undefined variable: hello
  --> :2:9

help:
  Check if the variable was declared in this scope.
- pure_fold: parse error: parse error: 2:10: unexpected token "is" (expected "!=" <ident>)
- pure_global_fold: parse error: parse error: 2:10: unexpected token "is" (expected "!=" <ident>)
- query_sum_select: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- record_assign: parse error: parse error: 2:9: unexpected token "(" (expected ")")
- right_join: compile error: unsupported join side
- save_jsonl_stdout: parse error: parse error: 2:59: lexer: invalid input text ";_14762=json), (..."
- short_circuit: parse error: parse error: 9:14: lexer: invalid input text ";_14130))\n}\n"
- slice: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- sort_stable: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- str_builtin: type error: error[T002]: undefined variable: _11082
  --> :3:9

help:
  Check if the variable was declared in this scope.
- string_compare: parse error: parse error: 2:10: lexer: invalid input text "@<b)\n  print(a@=..."
- string_concat: type error: error[T002]: undefined variable: _11192
  --> :3:9

help:
  Check if the variable was declared in this scope.
- string_contains: parse error: parse error: 2:69: lexer: invalid input text ";_13492=false)\nr..."
- string_in_operator: parse error: parse error: 2:69: lexer: invalid input text ";_13492=false)\nr..."
- string_index: parse error: parse error: 2:67: lexer: invalid input text ";_13052=_13036),..."
- string_prefix_slice: parse error: parse error: 2:19: unexpected token "!" (expected LogicCond)
- substring_builtin: type error: error[T002]: undefined variable: _11178
  --> :3:9

help:
  Check if the variable was declared in this scope.
- sum_builtin: parse error: parse error: 2:20: unexpected token "!" (expected LogicCond)
- tail_recursion: parse error: parse error: 2:42: lexer: invalid input text ";true), true), r..."
- test_block: parse error: parse error: 2:15: lexer: invalid input text ";throw(error(exp..."
- tree_sum: compile error: unsupported expression
- two-sum: parse error: parse error: 2:67: lexer: invalid input text ";_25268=_25252),..."
- typed_let: type error: error[T002]: undefined variable: Y
  --> :3:9

help:
  Check if the variable was declared in this scope.
- typed_var: type error: error[T002]: undefined variable: _11190
  --> :4:9

help:
  Check if the variable was declared in this scope.
- unary_neg: ok
- update_stmt: compile error: update of immutable variable not supported
- user_type_literal: type error: error[T002]: undefined variable: Bob
  --> :2:33

help:
  Check if the variable was declared in this scope.
- values_builtin: type error: error[T002]: undefined variable: _11508
  --> :5:9

help:
  Check if the variable was declared in this scope.
- var_assignment: type error: error[T002]: undefined variable: _11322
  --> :5:9

help:
  Check if the variable was declared in this scope.
- while_loop: parse error: parse error: 18:21: lexer: invalid input text "; true)\n        ..."
