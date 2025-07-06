# Errors

- append_builtin: type error: error[T002]: undefined variable: specialize
  --> :1:5

help:
  Check if the variable was declared in this scope.
- avg_builtin: line 14: cannot parse
    13| 
->  14| begin
    15| writeln(specialize _avgList<integer>(specialize TArray<integer>([1, 2, 3])));

- basic_compare: parse error: parse error: 4:10: unexpected token "=" (expected "(" (Expr ("," Expr)*)? ")")
- binary_precedence: ok
- bool_chain: line 13: cannot parse
    12| exit;
->  13| end;
    14| 

- break_continue: parse error: parse error: 5:7: lexer: invalid input text "'odd number:', n..."
- cast_string_to_int: parse error: parse error: 1:13: lexer: invalid input text "'1995'))"
- cast_struct: line 17: cannot parse
    16| _tmp0 := specialize TFPGMap<string, Variant>.Create;
->  17| _tmp0.AddOrSetData('title', 'hi');
    18| todo := Trunc(_tmp0);

- closure: line 12: cannot parse
    11| exit;
->  12| end;
    13| 

- count_builtin: line 13: cannot parse
    12| 
->  13| begin
    14| writeln(specialize _countList<integer>(specialize TArray<integer>([1, 2, 3])));

- cross_join: line 26: cannot parse
    25| _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
->  26| _tmp0.AddOrSetData('id', 1);
    27| _tmp0.AddOrSetData('name', 'Alice');

- cross_join_filter: line 22: cannot parse
    21| _tmp0 := specialize TFPGMap<integer, Variant>.Create;
->  22| _tmp0.AddOrSetData('n', n);
    23| _tmp0.AddOrSetData('l', l);

- cross_join_triple: line 25: cannot parse
    24| _tmp0 := specialize TFPGMap<integer, Variant>.Create;
->  25| _tmp0.AddOrSetData('n', n);
    26| _tmp0.AddOrSetData('l', l);

- dataset_sort_take_limit: line 22: cannot parse
    21| 
->  22| generic procedure _sortBy<T>(var arr: specialize TArray<T>; keys: specialize TArray<Variant>);
    23| var i,j: integer; tmp: T; k: Variant;

- dataset_where_filter: line 22: cannot parse
    21| _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
->  22| _tmp0.AddOrSetData('name', 'Alice');
    23| _tmp0.AddOrSetData('age', 30);

- exists_builtin: parse error: parse error: 3:12: unexpected token "=" (expected "(" (Expr ("," Expr)*)? ")")
- for_list_collection: parse error: parse error: 1:21: unexpected token "TArray" (expected "{" Statement* "}")
- for_loop: ok
- for_map_collection: line 15: cannot parse
    14| _tmp0 := specialize TFPGMap<string, integer>.Create;
->  15| _tmp0.AddOrSetData('a', 1);
    16| _tmp0.AddOrSetData('b', 2);

- fun_call: line 12: cannot parse
    11| exit;
->  12| end;
    13| 

- fun_expr_in_let: parse error: parse error: 1:10: lexer: invalid input text "@_lambda0\nprint(..."
- fun_three_args: line 12: cannot parse
    11| exit;
->  12| end;
    13| 

- group_by: line 19: cannot parse
    18| 
->  19| generic function _group_by<T>(src: specialize TArray<T>; keyfn: function(it: T): Variant): specialize TArray<specialize _Group<T>>;
    20| var i,j,idx: Integer; key: Variant; ks: string;

- group_by_conditional_sum: line 21: cannot parse
    20| generic function _sumList<T>(arr: specialize TArray<T>): double;
->  21| var i: integer; s: double;
    22| begin

- group_by_having: line 23: cannot parse
    22| for j := 0 to High(Result) do
->  23| if VarToStr(Result[j].Key) = ks then begin idx := j; Break; end;
    24| if idx = -1 then

- group_by_join: line 24: cannot parse
    23| _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
->  24| _tmp0.AddOrSetData('id', 1);
    25| _tmp0.AddOrSetData('name', 'Alice');

- group_by_left_join: line 13: cannot parse
    12| 
->  13| var
    14| _tmp0: specialize TFPGMap<Variant, Variant>;

- group_by_multi_join: line 23: cannot parse
    22| for j := 0 to High(Result) do
->  23| if VarToStr(Result[j].Key) = ks then begin idx := j; Break; end;
    24| if idx = -1 then

- group_by_multi_join_sort: line 21: cannot parse
    20| generic function _sumList<T>(arr: specialize TArray<T>): double;
->  21| var i: integer; s: double;
    22| begin

- group_by_sort: line 21: cannot parse
    20| generic function _sumList<T>(arr: specialize TArray<T>): double;
->  21| var i: integer; s: double;
    22| begin

- group_items_iteration: line 23: cannot parse
    22| for j := 0 to High(Result) do
->  23| if VarToStr(Result[j].Key) = ks then begin idx := j; Break; end;
    24| if idx = -1 then

- if_else: line 16: cannot parse
    15| writeln('big');
->  16| end else
    17| begin

- if_then_else: line 18: cannot parse
    17| _tmp0 := 'yes';
->  18| end else
    19| begin

- if_then_else_nested: line 18: cannot parse
    17| _tmp0 := 'big';
->  18| end else if (x > 5) then
    19| begin

- in_operator: type error: error[T002]: undefined variable: specialize
  --> :1:6

help:
  Check if the variable was declared in this scope.
- in_operator_extended: line 29: cannot parse
    28| _tmp1 := specialize TFPGMap<Variant, integer>.Create;
->  29| _tmp1.AddOrSetData('a', 1);
    30| m := _tmp1;

- inner_join: line 26: cannot parse
    25| _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
->  26| _tmp0.AddOrSetData('id', 1);
    27| _tmp0.AddOrSetData('name', 'Alice');

- join_multi: line 26: cannot parse
    25| _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
->  26| _tmp0.AddOrSetData('id', 1);
    27| _tmp0.AddOrSetData('name', 'Alice');

- json_builtin: line 13: cannot parse
    12| 
->  13| var
    14| _tmp0: specialize TFPGMap<Variant, integer>;

- left_join: line 23: cannot parse
    22| _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
->  23| _tmp0.AddOrSetData('id', 1);
    24| _tmp0.AddOrSetData('name', 'Alice');

- left_join_multi: line 25: cannot parse
    24| _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
->  25| _tmp0.AddOrSetData('id', 1);
    26| _tmp0.AddOrSetData('name', 'Alice');

- len_builtin: parse error: parse error: 1:25: unexpected token "TArray" (expected ")")
- len_map: line 13: cannot parse
    12| _tmp0 := specialize TFPGMap<string, integer>.Create;
->  13| _tmp0.AddOrSetData('a', 1);
    14| _tmp0.AddOrSetData('b', 2);

- len_string: parse error: parse error: 1:14: lexer: invalid input text "'mochi'))"
- let_and_print: type error: error[T001]: assignment to undeclared variable: a
  --> :1:1

help:
  Declare `a` first using `let`.
- list_assign: line 12: cannot parse
    11| if (i < 0) or (i >= Length(arr)) then
->  12| raise Exception.Create('index out of range');
    13| Result := arr[i];

- list_index: line 12: cannot parse
    11| if (i < 0) or (i >= Length(arr)) then
->  12| raise Exception.Create('index out of range');
    13| Result := arr[i];

- list_nested_assign: line 12: cannot parse
    11| if (i < 0) or (i >= Length(arr)) then
->  12| raise Exception.Create('index out of range');
    13| Result := arr[i];

- list_set_ops: compile error: unexpected expression state
- load_yaml: line 17: cannot parse
    16| sl := TStringList.Create;
->  17| try
    18| sl.LoadFromFile(path);

- map_assign: line 14: cannot parse
    13| _tmp0 := specialize TFPGMap<string, integer>.Create;
->  14| _tmp0.AddOrSetData('alice', 1);
    15| scores := _tmp0;

- map_in_operator: line 14: cannot parse
    13| _tmp0 := specialize TFPGMap<integer, Variant>.Create;
->  14| _tmp0.AddOrSetData(1, 'a');
    15| _tmp0.AddOrSetData(2, 'b');

- map_index: line 14: cannot parse
    13| _tmp0 := specialize TFPGMap<string, integer>.Create;
->  14| _tmp0.AddOrSetData('a', 1);
    15| _tmp0.AddOrSetData('b', 2);

- map_int_key: line 14: cannot parse
    13| _tmp0 := specialize TFPGMap<integer, Variant>.Create;
->  14| _tmp0.AddOrSetData(1, 'a');
    15| _tmp0.AddOrSetData(2, 'b');

- map_literal_dynamic: line 18: cannot parse
    17| _tmp0 := specialize TFPGMap<string, integer>.Create;
->  18| _tmp0.AddOrSetData('a', x);
    19| _tmp0.AddOrSetData('b', y);

- map_membership: line 14: cannot parse
    13| _tmp0 := specialize TFPGMap<string, integer>.Create;
->  14| _tmp0.AddOrSetData('a', 1);
    15| _tmp0.AddOrSetData('b', 2);

- map_nested_assign: line 16: cannot parse
    15| _tmp1 := specialize TFPGMap<string, integer>.Create;
->  16| _tmp1.AddOrSetData('inner', 1);
    17| _tmp0.AddOrSetData('outer', _tmp1);

- match_expr: line 20: cannot parse
    19| _tmp0 := 'one';
->  20| else if _tmp1 = 2 then
    21| begin

- match_full: line 17: cannot parse
    16| _tmp0 := 'zero';
->  17| else if _tmp1 = 1 then
    18| begin

- math_ops: parse error: parse error: 2:9: unexpected token "div" (expected ")")
- membership: type error: error[T002]: undefined variable: specialize
  --> :1:8

help:
  Check if the variable was declared in this scope.
- min_max_builtin: type error: error[T002]: undefined variable: specialize
  --> :1:8

help:
  Check if the variable was declared in this scope.
- nested_function: line 10: cannot parse
     9| begin
->  10| function inner(y: integer): integer;
    11| begin

- order_by_map: line 20: cannot parse
    19| 
->  20| var
    21| _tmp0: specialize TFPGMap<Variant, integer>;

- outer_join: line 27: cannot parse
    26| _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
->  27| _tmp0.AddOrSetData('id', 1);
    28| _tmp0.AddOrSetData('name', 'Alice');

- partial_application: line 12: cannot parse
    11| exit;
->  12| end;
    13| 

- print_hello: parse error: parse error: 1:7: lexer: invalid input text "'hello')"
- pure_fold: line 12: cannot parse
    11| exit;
->  12| end;
    13| 

- pure_global_fold: line 12: cannot parse
    11| exit;
->  12| end;
    13| 

- query_sum_select: type error: error[T002]: undefined variable: specialize
  --> :1:8

help:
  Check if the variable was declared in this scope.
- record_assign: line 14: cannot parse
    13| c := c.n + 1;
->  14| end;
    15| 

- right_join: line 26: cannot parse
    25| _tmp0 := specialize TFPGMap<Variant, Variant>.Create;
->  26| _tmp0.AddOrSetData('id', 1);
    27| _tmp0.AddOrSetData('name', 'Alice');

- save_jsonl_stdout: line 13: cannot parse
    12| ds := TJSONStreamer.Create(nil);
->  13| try
    14| for i := 0 to High(data) do

- short_circuit: line 13: cannot parse
    12| exit;
->  13| end;
    14| 

- slice: line 22: cannot parse
    21| 
->  22| function _sliceString(s: string; i, j: integer): string;
    23| var start_, end_, n: integer;

- sort_stable: line 20: cannot parse
    19| 
->  20| var
    21| _tmp0: specialize TFPGMap<Variant, Variant>;

- str_builtin: type error: error[T003]: unknown function: IntToStr
  --> :1:7

help:
  Ensure the function is defined before it's called.
- string_compare: parse error: parse error: 1:8: lexer: invalid input text "'a' < 'b'))\nprin..."
- string_concat: parse error: parse error: 1:7: lexer: invalid input text "'hello ' + 'worl..."
- string_contains: parse error: parse error: 1:5: lexer: invalid input text "'catch'\nprint(s...."
- string_in_operator: parse error: parse error: 1:5: lexer: invalid input text "'catch'\nprint(('..."
- string_index: line 12: cannot parse
    11| if (i < 0) or (i >= Length(s)) then
->  12| raise Exception.Create('index out of range');
    13| Result := s[i + 1];

- string_prefix_slice: line 20: cannot parse
    19| Result := Copy(s, start_ + 1, end_ - start_);
->  20| end;
    21| 

- substring_builtin: line 20: cannot parse
    19| Result := Copy(s, start_ + 1, end_ - start_);
->  20| end;
    21| 

- sum_builtin: line 17: cannot parse
    16| 
->  17| begin
    18| writeln(specialize _sumList<integer>(specialize TArray<integer>([1, 2, 3])));

- tail_recursion: line 15: cannot parse
    14| 
->  15| begin
    16| writeln(sum_rec(10, 0));

- test_block: line 16: cannot parse
    15| 
->  16| begin
    17| writeln('ok');

- tree_sum: compile error: union types not supported
- two-sum: line 26: cannot parse
    25| 
->  26| generic function _indexList<T>(arr: specialize TArray<T>; i: integer): T;
    27| begin

- typed_let: compile error: nil expr
- typed_var: type error: error[T002]: undefined variable: x
  --> :1:7

help:
  Check if the variable was declared in this scope.
- unary_neg: parse error: parse error: 2:11: unexpected token "-" (expected PostfixExpr)
- update_stmt: line 44: cannot parse
    43| 
->  44| begin
    45| _tmp0.name := 'Alice';

- user_type_literal: parse error: parse error: 9:15: lexer: invalid input text "'Go'\n_tmp1.name ..."
- values_builtin: line 14: cannot parse
    13| _tmp0 := specialize TFPGMap<string, integer>.Create;
->  14| _tmp0.AddOrSetData('a', 1);
    15| _tmp0.AddOrSetData('b', 2);

- var_assignment: type error: error[T001]: assignment to undeclared variable: x
  --> :1:1

help:
  Declare `x` first using `let`.
- while_loop: type error: error[T001]: assignment to undeclared variable: i
  --> :1:1

help:
  Declare `i` first using `let`.
