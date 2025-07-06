# Prolog roundtrip VM test failures

## tests/vm/valid/append_builtin.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run1730231765/main.pl:8: user:main main/0: Arguments are not sufficiently instantiated

```

## tests/vm/valid/basic_compare.mochi

```
output mismatch
-- pl --
7
7=7
4<5
-- vm --
7
true
true
```

## tests/vm/valid/bool_chain.mochi

```
output mismatch
-- pl --
(1<2,2<3),3<4
boom
(1<2,2>3),_3896
boom
((1<2,2<3),3>4),_3966
-- vm --
true
false
false
```

## tests/vm/valid/closure.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run2552459391/main.pl:16: user:main is/2: Arguments are not sufficiently instantiated

```

## tests/vm/valid/dataset_where_filter.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/exists_builtin.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run3124822022/main.pl:18: user:main main/0: Arguments are not sufficiently instantiated

```

## tests/vm/valid/group_by.mochi

```
output mismatch
-- pl --
--- People grouped by city ---
Hanoi : count = 3 , avg_age = 27.333333333333332
Paris : count = 3 , avg_age = 55
-- vm --
--- People grouped by city ---
Paris : count = 3 , avg_age = 55
Hanoi : count = 3 , avg_age = 27.333333333333332
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/group_by_having.mochi

```
output mismatch
-- pl --
[ {"city":"Hanoi", "num":3},  {"city":"Paris", "num":4} ]
-- vm --
[{"city":"Paris","num":4}]
```

## tests/vm/valid/group_by_join.mochi

```
output mismatch
-- pl --
--- Orders per customer ---
Bob orders: 1
Alice orders: 2
-- vm --
--- Orders per customer ---
Alice orders: 2
Bob orders: 1
```

## tests/vm/valid/group_by_left_join.mochi

```
output mismatch
-- pl --
--- Group Left Join ---
Bob orders: 0
Alice orders: 0
-- vm --
--- Group Left Join ---
Alice orders: 2
Bob orders: 1
Charlie orders: 0
```

## tests/vm/valid/group_by_multi_join.mochi

```
output mismatch
-- pl --
[map{part:200,total:15},map{part:100,total:20}]
-- vm --
map[part:100 total:20] map[part:200 total:15]
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run4203205978/main.pl:49: user:main >=/2: Type error: `[]' expected, found `"1993-10-15"' (a string) ("x" must hold one character)

```

## tests/vm/valid/group_by_sort.mochi

```
output mismatch
-- pl --
[map{cat:b,total:7},map{cat:a,total:4}]
-- vm --
map[cat:b total:7] map[cat:a total:4]
```

## tests/vm/valid/group_items_iteration.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run912115736/main.pl:80: user:main get_dict/3: Type error: `dict' expected, found `[map{tag:"b",val:3}]' (a list)

```

## tests/vm/valid/if_then_else.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/if_then_else_nested.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/in_operator_extended.mochi

```
swipl error: exit status 2
true
false
ERROR: /tmp/pl-run3375205022/main.pl:43: user:main get_dict/3: Type error: `dict-key' expected, found `"a"' (a string)

```

## tests/vm/valid/json_builtin.mochi

```
output mismatch
-- pl --
{"a":1, "b":2}
-- vm --
{"a":1,"b":2}
```

## tests/vm/valid/left_join.mochi

```
compile error: unsupported join side
```

## tests/vm/valid/left_join_multi.mochi

```
compile error: unsupported join side
```

## tests/vm/valid/len_map.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run2079212670/main.pl:8: user:main length/2: Type error: `list' expected, found `map{a:1,b:2}' (a dict)

```

## tests/vm/valid/len_string.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run1394079141/main.pl:7: user:main length/2: Type error: `list' expected, found `"mochi"' (a string)

```

## tests/vm/valid/list_set_ops.mochi

```
output mismatch
-- pl --
[1,2,3]
[1,3]
[2]
4
-- vm --
1 2 3
1 3
2
4
```

## tests/vm/valid/load_yaml.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run1018026454/main.pl:52: user:main source_sink `"../interpreter/valid/people.yaml"' does not exist

```

## tests/vm/valid/map_membership.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run2539524793/main.pl:19: user:main get_dict/3: Type error: `dict-key' expected, found `"a"' (a string)

```

## tests/vm/valid/match_expr.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/match_full.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/min_max_builtin.mochi

```
swipl error: exit status 2
1
ERROR: /tmp/pl-run660940679/main.pl:18: user:main main/0: Arguments are not sufficiently instantiated

```

## tests/vm/valid/nested_function.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run3089440667/main.pl:7:24: Syntax error: Operator expected
ERROR: /tmp/pl-run3089440667/main.pl:8:20: Syntax error: Operand expected, unquoted comma or bar found
ERROR: /tmp/pl-run3089440667/main.pl:24: user:main outer/2: Unknown procedure: inner/2

```

## tests/vm/valid/order_by_map.mochi

```
output mismatch
-- pl --
[map{a:0,b:5},map{a:1,b:1},map{a:1,b:2}]
-- vm --
map[a:0 b:5] map[a:1 b:1] map[a:1 b:2]
```

## tests/vm/valid/outer_join.mochi

```
compile error: unsupported join side
```

## tests/vm/valid/partial_application.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run13854057/main.pl:13: user:main main/0: Unknown procedure: add/2
ERROR:   However, there are definitions for:
ERROR:         add/3

```

## tests/vm/valid/pure_global_fold.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run3301029534/main.pl:12: user:main is/2: Arguments are not sufficiently instantiated

```

## tests/vm/valid/query_sum_select.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run3770043451/main.pl:24: user:main Unknown message: error('sum expects list or group')

```

## tests/vm/valid/record_assign.mochi

```
swipl error: exit status 1

```

## tests/vm/valid/right_join.mochi

```
compile error: unsupported join side
```

## tests/vm/valid/save_jsonl_stdout.mochi

```
output mismatch
-- pl --

-- vm --
{"age":30,"name":"Alice"}
{"age":25,"name":"Bob"}
```

## tests/vm/valid/short_circuit.mochi

```
output mismatch
-- pl --
boom
false,_3866
boom
true;_3918
-- vm --
false
true
```

## tests/vm/valid/slice.mochi

```
output mismatch
-- pl --
[2,3]
[1,2]
ell
-- vm --
2 3
1 2
ell
```

## tests/vm/valid/sort_stable.mochi

```
output mismatch
-- pl --
[a,b,c]
-- vm --
a b c
```

## tests/vm/valid/string_compare.mochi

```
output mismatch
-- pl --
a@<b
a@=<a
b@>a
b@>=b
-- vm --
true
true
true
true
```

## tests/vm/valid/string_contains.mochi

```
output mismatch
-- pl --
false
false
-- vm --
true
false
```

## tests/vm/valid/string_in_operator.mochi

```
output mismatch
-- pl --
false
false
-- vm --
true
false
```

## tests/vm/valid/string_prefix_slice.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run144620682/main.pl:27: user:main length/2: Type error: `list' expected, found `"fore"' (a string)

```

## tests/vm/valid/substring_builtin.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run952602207/main.pl:7: user:main main/0: Arguments are not sufficiently instantiated

```

## tests/vm/valid/tail_recursion.mochi

```
output mismatch
-- pl --
_3866
-- vm --
55
```

## tests/vm/valid/tree_sum.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/typed_let.mochi

```
output mismatch
-- pl --
ERROR: /tmp/pl-run2212243628/main.pl:3:7: Syntax error: Operand expected, unquoted comma or bar found
-- vm --
<nil>
```

## tests/vm/valid/typed_var.mochi

```
output mismatch
-- pl --
_3868
-- vm --
<nil>
```

## tests/vm/valid/update_stmt.mochi

```
compile error: update of immutable variable not supported
```

## tests/vm/valid/values_builtin.mochi

```
swipl error: exit status 2
ERROR: /tmp/pl-run1155604352/main.pl:9: user:main main/0: Arguments are not sufficiently instantiated

```

## tests/vm/valid/while_loop.mochi

```
output mismatch
-- pl --
ERROR: /tmp/pl-run3103276892/main.pl:17:23: Syntax error: Unbalanced operator
-- vm --
0
1
2
```

