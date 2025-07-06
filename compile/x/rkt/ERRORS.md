# Racket compiler VM comparison failures

## tests/vm/valid/append_builtin.mochi

```
output mismatch
-- racket --
(1 2 . 3)
-- vm --
1 2 3
```

## tests/vm/valid/avg_builtin.mochi

```
output mismatch
-- racket --
2.0
-- vm --
2
```

## tests/vm/valid/basic_compare.mochi

```
output mismatch
-- racket --
7
#t
#t
-- vm --
7
true
true
```

## tests/vm/valid/bool_chain.mochi

```
output mismatch
-- racket --
#t
#f
#f
-- vm --
true
false
false
```

## tests/vm/valid/cross_join_triple.mochi

```
output mismatch
-- racket --
--- Cross Join of three lists ---
1 A #t
1 A #f
1 B #t
1 B #f
2 A #t
2 A #f
2 B #t
2 B #f
-- vm --
--- Cross Join of three lists ---
1 A true
1 A false
1 B true
1 B false
2 A true
2 A false
2 B true
2 B false
```

## tests/vm/valid/dataset_where_filter.mochi

```
output mismatch
-- racket --
--- Adults ---
Alice is 30 
Charlie is 65  (senior)
Diana is 45
-- vm --
--- Adults ---
Alice is 30
Charlie is 65  (senior)
Diana is 45
```

## tests/vm/valid/exists_builtin.mochi

```
racket run error: exit status 1
/tmp/rkt283859949/main.rkt:51:14: exists: unbound identifier
  in: exists
  location...:
   /tmp/rkt283859949/main.rkt:51:14

```

## tests/vm/valid/for_map_collection.mochi

```
output mismatch
-- racket --
b
a
-- vm --
a
b
```

## tests/vm/valid/group_by.mochi

```
racket run error: exit status 1
length: contract violation
  expected: list?
  given: #<_Group>
  context...:
   body of "/tmp/rkt2294232451/main.rkt"

```

## tests/vm/valid/group_by_conditional_sum.mochi

```
racket run error: exit status 1
for: expected a sequence for x, got something else: #<_Group>
  context...:
   /usr/share/racket/collects/racket/private/for.rkt:557:2: make-sequence
   body of "/tmp/rkt4278034721/main.rkt"

```

## tests/vm/valid/group_by_having.mochi

```
racket run error: exit status 1
length: contract violation
  expected: list?
  given: #<_Group>
  context...:
   body of "/tmp/rkt4024124212/main.rkt"

```

## tests/vm/valid/group_by_join.mochi

```
racket run error: exit status 1
length: contract violation
  expected: list?
  given: #<_Group>
  context...:
   body of "/tmp/rkt3552168512/main.rkt"

```

## tests/vm/valid/group_by_left_join.mochi

```
racket run error: exit status 1
for: expected a sequence for r, got something else: #<_Group>
  context...:
   /usr/share/racket/collects/racket/private/for.rkt:557:2: make-sequence
   body of "/tmp/rkt3664594473/main.rkt"

```

## tests/vm/valid/group_by_multi_join.mochi

```
racket run error: exit status 1
for: expected a sequence for r, got something else: #<_Group>
  context...:
   /usr/share/racket/collects/racket/private/for.rkt:557:2: make-sequence
   body of "/tmp/rkt165168038/main.rkt"

```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
racket run error: exit status 1
for: expected a sequence for x, got something else: #<_Group>
  context...:
   /usr/share/racket/collects/racket/private/for.rkt:557:2: make-sequence
   body of "/tmp/rkt2438042999/main.rkt"

```

## tests/vm/valid/group_by_sort.mochi

```
racket run error: exit status 1
for: expected a sequence for x, got something else: #<_Group>
  context...:
   /usr/share/racket/collects/racket/private/for.rkt:557:2: make-sequence
   body of "/tmp/rkt1990373765/main.rkt"

```

## tests/vm/valid/group_items_iteration.mochi

```
racket run error: exit status 1
hash-ref: contract violation
  expected: hash?
  given: #<_Group>
  argument position: 1st
  other arguments...:
   "items"
  context...:
   /usr/share/racket/collects/racket/private/for.rkt:1539:9
   body of "/tmp/rkt1363788557/main.rkt"

```

## tests/vm/valid/if_then_else_nested.mochi

```
output mismatch
-- racket --
big
-- vm --
medium
```

## tests/vm/valid/in_operator.mochi

```
output mismatch
-- racket --
#t
#t
-- vm --
true
true
```

## tests/vm/valid/in_operator_extended.mochi

```
output mismatch
-- racket --
#t
#f
#t
#f
#t
#f
-- vm --
true
false
true
false
true
false
```

## tests/vm/valid/left_join.mochi

```
compile error: join sides not supported
```

## tests/vm/valid/left_join_multi.mochi

```
compile error: join sides not supported
```

## tests/vm/valid/list_set_ops.mochi

```
racket run error: exit status 1
/tmp/rkt1115209112/main.rkt:50:33: list->list: unbound identifier
  in: list->list
  location...:
   /tmp/rkt1115209112/main.rkt:50:33

```

## tests/vm/valid/load_yaml.mochi

```
racket run error: exit status 1
opts: undefined;
 cannot use before initialization
  context...:
   /tmp/rkt3771282013/main.rkt:69:0: _load
   body of "/tmp/rkt3771282013/main.rkt"

```

## tests/vm/valid/map_in_operator.mochi

```
output mismatch
-- racket --
#t
#f
-- vm --
true
false
```

## tests/vm/valid/map_membership.mochi

```
output mismatch
-- racket --
#t
#f
-- vm --
true
false
```

## tests/vm/valid/math_ops.mochi

```
output mismatch
-- racket --
42
3
1
-- vm --
42
3.5
1
```

## tests/vm/valid/membership.mochi

```
output mismatch
-- racket --
#t
#f
-- vm --
true
false
```

## tests/vm/valid/min_max_builtin.mochi

```
racket run error: exit status 1
max: contract violation
  expected: real?
  given: '(3 1 4)
  context...:
   body of "/tmp/rkt3506751823/main.rkt"
1

```

## tests/vm/valid/order_by_map.mochi

```
output mismatch
-- racket --
(#hash((a . 0) (b . 5)) #hash((a . 1) (b . 1)) #hash((a . 1) (b . 2)))
-- vm --
map[a:0 b:5] map[a:1 b:1] map[a:1 b:2]
```

## tests/vm/valid/outer_join.mochi

```
compile error: join sides not supported
```

## tests/vm/valid/partial_application.mochi

```
racket run error: exit status 1
add: arity mismatch;
 the expected number of arguments does not match the given number
  expected: 2
  given: 1
  context...:
   body of "/tmp/rkt3666910256/main.rkt"

```

## tests/vm/valid/query_sum_select.mochi

```
output mismatch
-- racket --
(1.0 3.0)
-- vm --
5
```

## tests/vm/valid/record_assign.mochi

```
output mismatch
-- racket --
0
-- vm --
1
```

## tests/vm/valid/right_join.mochi

```
compile error: join sides not supported
```

## tests/vm/valid/save_jsonl_stdout.mochi

```
racket run error: exit status 1
opts: undefined;
 cannot use before initialization
  context...:
   /tmp/rkt1333680944/main.rkt:77:0: _save
   body of "/tmp/rkt1333680944/main.rkt"

```

## tests/vm/valid/short_circuit.mochi

```
output mismatch
-- racket --
#f
#t
-- vm --
false
true
```

## tests/vm/valid/slice.mochi

```
output mismatch
-- racket --
(2 3)
(1 2)
ell
-- vm --
2 3
1 2
ell
```

## tests/vm/valid/sort_stable.mochi

```
output mismatch
-- racket --
(a b c)
-- vm --
a b c
```

## tests/vm/valid/string_compare.mochi

```
output mismatch
-- racket --
#t
#t
#t
#t
-- vm --
true
true
true
true
```

## tests/vm/valid/string_contains.mochi

```
output mismatch
-- racket --
#t
#f
-- vm --
true
false
```

## tests/vm/valid/string_in_operator.mochi

```
output mismatch
-- racket --
#t
#f
-- vm --
true
false
```

## tests/vm/valid/string_index.mochi

```
output mismatch
-- racket --
111
-- vm --
o
```

## tests/vm/valid/string_prefix_slice.mochi

```
output mismatch
-- racket --
#t
#f
-- vm --
true
false
```

## tests/vm/valid/sum_builtin.mochi

```
output mismatch
-- racket --
6.0
-- vm --
6
```

## tests/vm/valid/tree_sum.mochi

```
racket run error: exit status 1
/tmp/rkt771963891/main.rkt:56:3: match: syntax error in pattern
  in: (Node left value right)
  location...:
   /tmp/rkt771963891/main.rkt:56:3
  context...:
   /usr/share/racket/collects/racket/match/gen-match.rkt:55:11: mk
   /usr/share/racket/collects/racket/match/../../syntax/parse/private/parse-interp.rkt:643:50
   /usr/share/racket/collects/racket/match/gen-match.rkt:24:0: go

```

## tests/vm/valid/typed_let.mochi

```
output mismatch
-- racket --
#<void>
-- vm --
<nil>
```

## tests/vm/valid/typed_var.mochi

```
output mismatch
-- racket --
#<void>
-- vm --
<nil>
```

## tests/vm/valid/update_stmt.mochi

```
compile error: unsupported statement
```

## tests/vm/valid/values_builtin.mochi

```
output mismatch
-- racket --
#hash((a . 1) (b . 2) (c . 3))
-- vm --
1 2 3
```

