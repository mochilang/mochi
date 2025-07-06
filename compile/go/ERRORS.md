# Go roundtrip VM test failures

## tests/vm/valid/avg_builtin.mochi

```
type roundtrip error: error[T003]: unknown function: float64
  --> :7:15

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/cast_string_to_int.mochi

```
go2mochi error: tests/vm/valid/cast_string_to_int.go.out:17: unsupported statement *ast.TypeSwitchStmt
>>> 16:    var out T
17:>>> switch any(out).(type) {
18:    case int:
19:    switch vv := v.(type) {
```

## tests/vm/valid/cast_struct.mochi

```
go2mochi error: tests/vm/valid/cast_struct.go.out:23: unsupported statement *ast.TypeSwitchStmt
>>> 22:    var out T
23:>>> switch any(out).(type) {
24:    case int:
25:    switch vv := v.(type) {
```

## tests/vm/valid/closure.mochi

```
parse roundtrip error: parse error: 1:28: unexpected token "(" (expected "{" Statement* "}")
```

## tests/vm/valid/cross_join.mochi

```
go2mochi error: tests/vm/valid/cross_join.go.out:8: unsupported declaration
>>> 7:    func main() {
8:>>> type CustomersItem struct {
9:    Id   int    `json:"id"`
10:    Name string `json:"name"`
```

## tests/vm/valid/cross_join_filter.mochi

```
parse roundtrip error: parse error: 4:41: unexpected token ">" (expected "<" TypeRef ("," TypeRef)* ">")
```

## tests/vm/valid/cross_join_triple.mochi

```
parse roundtrip error: parse error: 6:42: unexpected token ">" (expected "<" TypeRef ("," TypeRef)* ">")
```

## tests/vm/valid/dataset_sort_take_limit.mochi

```
go2mochi error: tests/vm/valid/dataset_sort_take_limit.go.out:65: unsupported statement *ast.TypeSwitchStmt
>>> 64:    var out T
65:>>> switch any(out).(type) {
66:    case int:
67:    switch vv := v.(type) {
```

## tests/vm/valid/dataset_where_filter.mochi

```
go2mochi error: tests/vm/valid/dataset_where_filter.go.out:8: unsupported declaration
>>> 7:    func main() {
8:>>> type PeopleItem struct {
9:    Name string `json:"name"`
10:    Age  int    `json:"age"`
```

## tests/vm/valid/exists_builtin.mochi

```
parse roundtrip error: parse error: 2:34: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/fun_expr_in_let.mochi

```
parse roundtrip error: parse error: 1:32: unexpected token "{" (expected "<" TypeRef ("," TypeRef)* ">")
```

## tests/vm/valid/group_by.mochi

```
go2mochi error: tests/vm/valid/group_by.go.out:90: unsupported statement *ast.TypeSwitchStmt
>>> 89:    } else {
90:>>> switch s := v.(type) {
91:    case []any:
92:    items = s
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
go2mochi error: tests/vm/valid/group_by_conditional_sum.go.out:120: unsupported statement *ast.TypeSwitchStmt
>>> 119:    var out T
120:>>> switch any(out).(type) {
121:    case int:
122:    switch vv := v.(type) {
```

## tests/vm/valid/group_by_having.mochi

```
go2mochi error: tests/vm/valid/group_by_having.go.out:10: unsupported declaration
>>> 9:    func main() {
10:>>> type PeopleItem struct {
11:    Name string `json:"name"`
12:    City string `json:"city"`
```

## tests/vm/valid/group_by_join.mochi

```
go2mochi error: tests/vm/valid/group_by_join.go.out:9: unsupported declaration
>>> 8:    func main() {
9:>>> type CustomersItem struct {
10:    Id   int    `json:"id"`
11:    Name string `json:"name"`
```

## tests/vm/valid/group_by_left_join.mochi

```
go2mochi error: tests/vm/valid/group_by_left_join.go.out:100: unsupported statement *ast.TypeSwitchStmt
>>> 99:    var out T
100:>>> switch any(out).(type) {
101:    case int:
102:    switch vv := v.(type) {
```

## tests/vm/valid/group_by_multi_join.mochi

```
go2mochi error: tests/vm/valid/group_by_multi_join.go.out:115: unsupported statement *ast.TypeSwitchStmt
>>> 114:    var out T
115:>>> switch any(out).(type) {
116:    case int:
117:    switch vv := v.(type) {
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
go2mochi error: tests/vm/valid/group_by_multi_join_sort.go.out:207: unsupported statement *ast.TypeSwitchStmt
>>> 206:    var out T
207:>>> switch any(out).(type) {
208:    case int:
209:    switch vv := v.(type) {
```

## tests/vm/valid/group_by_sort.mochi

```
go2mochi error: tests/vm/valid/group_by_sort.go.out:118: unsupported statement *ast.TypeSwitchStmt
>>> 117:    var out T
118:>>> switch any(out).(type) {
119:    case int:
120:    switch vv := v.(type) {
```

## tests/vm/valid/group_items_iteration.mochi

```
compile error: cannot iterate over type any
```

## tests/vm/valid/if_then_else.mochi

```
parse roundtrip error: parse error: 3:26: unexpected token "{" (expected "<" TypeRef ("," TypeRef)* ">")
```

## tests/vm/valid/if_then_else_nested.mochi

```
parse roundtrip error: parse error: 3:26: unexpected token "{" (expected "<" TypeRef ("," TypeRef)* ">")
```

## tests/vm/valid/in_operator.mochi

```
type roundtrip error: error[T002]: undefined variable: slices
  --> :2:7

help:
  Check if the variable was declared in this scope.
```

## tests/vm/valid/in_operator_extended.mochi

```
go2mochi error: tests/vm/valid/in_operator_extended.go.out:27: unsupported assignment
>>> 26:    _tmp1 := m
27:>>> _, _tmp2 := _tmp1[_tmp0]
28:    fmt.Println(_tmp2)
29:    _tmp3 := "b"
```

## tests/vm/valid/inner_join.mochi

```
go2mochi error: tests/vm/valid/inner_join.go.out:8: unsupported declaration
>>> 7:    func main() {
8:>>> type CustomersItem struct {
9:    Id   int    `json:"id"`
10:    Name string `json:"name"`
```

## tests/vm/valid/join_multi.mochi

```
go2mochi error: tests/vm/valid/join_multi.go.out:8: unsupported declaration
>>> 7:    func main() {
8:>>> type CustomersItem struct {
9:    Id   int    `json:"id"`
10:    Name string `json:"name"`
```

## tests/vm/valid/json_builtin.mochi

```
go2mochi error: tests/vm/valid/json_builtin.go.out:10: unsupported assignment
>>> 9:    var m map[string]int = map[string]int{"a": 1, "b": 2}
10:>>> func() { b, _ := json.Marshal(m); fmt.Println(string(b)) }()
11:    }
12:    
```

## tests/vm/valid/left_join.mochi

```
go2mochi error: tests/vm/valid/left_join.go.out:76: unsupported statement *ast.TypeSwitchStmt
>>> 75:    var out T
76:>>> switch any(out).(type) {
77:    case int:
78:    switch vv := v.(type) {
```

## tests/vm/valid/left_join_multi.mochi

```
go2mochi error: tests/vm/valid/left_join_multi.go.out:94: unsupported statement *ast.TypeSwitchStmt
>>> 93:    var out T
94:>>> switch any(out).(type) {
95:    case int:
96:    switch vv := v.(type) {
```

## tests/vm/valid/list_set_ops.mochi

```
go2mochi error: tests/vm/valid/list_set_ops.go.out:14: unsupported parameter list
>>> 13:    
14:>>> func _except[T any](a, b []T) []T {
15:    res := []T{}
16:    for _, x := range a {
```

## tests/vm/valid/load_yaml.mochi

```
go2mochi error: tests/vm/valid/load_yaml.go.out:46: unsupported statement *ast.TypeSwitchStmt
>>> 45:    var out T
46:>>> switch any(out).(type) {
47:    case int:
48:    switch vv := v.(type) {
```

## tests/vm/valid/map_in_operator.mochi

```
go2mochi error: tests/vm/valid/map_in_operator.go.out:11: unsupported assignment
>>> 10:    _tmp1 := m
11:>>> _, _tmp2 := _tmp1[_tmp0]
12:    fmt.Println(_tmp2)
13:    _tmp3 := 3
```

## tests/vm/valid/map_membership.mochi

```
go2mochi error: tests/vm/valid/map_membership.go.out:11: unsupported assignment
>>> 10:    _tmp1 := m
11:>>> _, _tmp2 := _tmp1[_tmp0]
12:    fmt.Println(_tmp2)
13:    _tmp3 := "c"
```

## tests/vm/valid/match_expr.mochi

```
go2mochi error: tests/vm/valid/match_expr.go.out:26: unsupported parameter list
>>> 25:    
26:>>> func _equal(a, b any) bool {
27:    av := reflect.ValueOf(a)
28:    bv := reflect.ValueOf(b)
```

## tests/vm/valid/match_full.mochi

```
go2mochi error: tests/vm/valid/match_full.go.out:70: unsupported parameter list
>>> 69:    
70:>>> func _equal(a, b any) bool {
71:    av := reflect.ValueOf(a)
72:    bv := reflect.ValueOf(b)
```

## tests/vm/valid/membership.mochi

```
type roundtrip error: error[T002]: undefined variable: slices
  --> :2:7

help:
  Check if the variable was declared in this scope.
```

## tests/vm/valid/nested_function.mochi

```
parse roundtrip error: parse error: 2:33: unexpected token "{" (expected "<" TypeRef ("," TypeRef)* ">")
```

## tests/vm/valid/order_by_map.mochi

```
go2mochi error: tests/vm/valid/order_by_map.go.out:42: unsupported statement *ast.TypeSwitchStmt
>>> 41:    var out T
42:>>> switch any(out).(type) {
43:    case int:
44:    switch vv := v.(type) {
```

## tests/vm/valid/outer_join.mochi

```
go2mochi error: tests/vm/valid/outer_join.go.out:104: unsupported statement *ast.TypeSwitchStmt
>>> 103:    var out T
104:>>> switch any(out).(type) {
105:    case int:
106:    switch vv := v.(type) {
```

## tests/vm/valid/pure_global_fold.mochi

```
type roundtrip error: error[T001]: assignment to undeclared variable: _
  --> :5:1

help:
  Declare `_` first using `let`.
```

## tests/vm/valid/query_sum_select.mochi

```
go2mochi error: tests/vm/valid/query_sum_select.go.out:29: unsupported statement *ast.TypeSwitchStmt
>>> 28:    } else {
29:>>> switch s := v.(type) {
30:    case []any:
31:    items = s
```

## tests/vm/valid/record_assign.mochi

```
go2mochi error: tests/vm/valid/record_assign.go.out:28: unsupported statement *ast.TypeSwitchStmt
>>> 27:    var out T
28:>>> switch any(out).(type) {
29:    case int:
30:    switch vv := v.(type) {
```

## tests/vm/valid/right_join.mochi

```
go2mochi error: tests/vm/valid/right_join.go.out:91: unsupported statement *ast.TypeSwitchStmt
>>> 90:    var out T
91:>>> switch any(out).(type) {
92:    case int:
93:    switch vv := v.(type) {
```

## tests/vm/valid/save_jsonl_stdout.mochi

```
go2mochi error: tests/vm/valid/save_jsonl_stdout.go.out:27: unsupported assignment
>>> 26:    func _save(src any, path string, opts map[string]any) {
27:>>> rows, ok := _toMapSlice(src)
28:    if !ok {
29:    panic("save source must be list of maps")
```

## tests/vm/valid/slice.mochi

```
go2mochi error: tests/vm/valid/slice.go.out:13: unsupported parameter list
>>> 12:    
13:>>> func _sliceString(s string, i, j int) string {
14:    start := i
15:    end := j
```

## tests/vm/valid/sort_stable.mochi

```
go2mochi error: tests/vm/valid/sort_stable.go.out:42: unsupported statement *ast.TypeSwitchStmt
>>> 41:    var out T
42:>>> switch any(out).(type) {
43:    case int:
44:    switch vv := v.(type) {
```

## tests/vm/valid/string_contains.mochi

```
type roundtrip error: error[T001]: assignment to undeclared variable: _
  --> :2:1

help:
  Declare `_` first using `let`.
```

## tests/vm/valid/string_in_operator.mochi

```
type roundtrip error: error[T002]: undefined variable: strings
  --> :2:7

help:
  Check if the variable was declared in this scope.
```

## tests/vm/valid/string_prefix_slice.mochi

```
go2mochi error: tests/vm/valid/string_prefix_slice.go.out:15: unsupported parameter list
>>> 14:    
15:>>> func _sliceString(s string, i, j int) string {
16:    start := i
17:    end := j
```

## tests/vm/valid/sum_builtin.mochi

```
type roundtrip error: error[T003]: unknown function: float64
  --> :4:15

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/test_block.mochi

```
go2mochi error: tests/vm/valid/test_block.go.out:48: unsupported statement *ast.BlockStmt
>>> 47:    fmt.Println("ok")
48:>>> {
49:    printTestStart("addition works")
50:    start := time.Now()
```

## tests/vm/valid/tree_sum.mochi

```
go2mochi error: tests/vm/valid/tree_sum.go.out:8: unsupported type spec
>>> 7:    
8:>>> type Tree interface{ isTree() }
9:    type Leaf struct {
10:    }
```

## tests/vm/valid/typed_let.mochi

```
type roundtrip error: error[T002]: undefined variable: nil
  --> :1:9

help:
  Check if the variable was declared in this scope.
```

## tests/vm/valid/typed_var.mochi

```
type roundtrip error: error[T002]: undefined variable: nil
  --> :1:9

help:
  Check if the variable was declared in this scope.
```

## tests/vm/valid/update_stmt.mochi

```
go2mochi error: tests/vm/valid/update_stmt.go.out:55: unsupported declaration
>>> 54:    
55:>>> var people []Person
56:    
57:    func main() {
```

## tests/vm/valid/user_type_literal.mochi

```
type roundtrip error: error[T001]: assignment to undeclared variable: _
  --> :10:1

help:
  Declare `_` first using `let`.
```

