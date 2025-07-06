# Python roundtrip VM test failures

## tests/vm/valid/append_builtin.mochi

```
parse roundtrip error: parse error: 4:9: unexpected token "*" (expected ")")
```

## tests/vm/valid/avg_builtin.mochi

```
panic: runtime error: index out of range [0] with length 0
```

## tests/vm/valid/basic_compare.mochi

```
parse roundtrip error: parse error: 4:8: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/binary_precedence.mochi

```
output mismatch
-- got --
7
7
7
7
-- want --
7
9
7
8
```

## tests/vm/valid/bool_chain.mochi

```
parse roundtrip error: parse error: 5:8: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/cast_string_to_int.mochi

```
type roundtrip error: error[T003]: unknown function: int
  --> :1:7

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/cast_struct.mochi

```
parse roundtrip error: parse error: 4:18: unexpected token ":" (expected "}")
```

## tests/vm/valid/closure.mochi

```
parse roundtrip error: parse error: 1:30: unexpected token "." (expected "{" Statement* "}")
```

## tests/vm/valid/cross_join.mochi

```
parse roundtrip error: parse error: 7:134: unexpected token "for" (expected "]")
```

## tests/vm/valid/cross_join_filter.mochi

```
parse roundtrip error: parse error: 2:16: lexer: invalid input text "'A', 'B']\nlet pa..."
```

## tests/vm/valid/cross_join_triple.mochi

```
parse roundtrip error: parse error: 2:16: lexer: invalid input text "'A', 'B']\nlet bo..."
```

## tests/vm/valid/dataset_sort_take_limit.mochi

```
parse roundtrip error: parse error: 4:25: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
```

## tests/vm/valid/dataset_where_filter.mochi

```
panic: runtime error: index out of range [0] with length 0
```

## tests/vm/valid/exists_builtin.mochi

```
parse roundtrip error: parse error: 3:8: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/for_loop.mochi

```
output mismatch
-- got --

-- want --
1
2
3
```

## tests/vm/valid/fun_expr_in_let.mochi

```
output mismatch
-- got --
<nil>
-- want --
36
```

## tests/vm/valid/group_by.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/group_by_having.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/group_by_join.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/group_by_left_join.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/group_by_multi_join.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/group_by_sort.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/group_items_iteration.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/if_then_else.mochi

```
output mismatch
-- got --
<nil>
-- want --
yes
```

## tests/vm/valid/if_then_else_nested.mochi

```
output mismatch
-- got --
<nil>
-- want --
medium
```

## tests/vm/valid/in_operator.mochi

```
parse roundtrip error: parse error: 2:8: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/in_operator_extended.mochi

```
parse roundtrip error: parse error: 4:8: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/inner_join.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/join_multi.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/json_builtin.mochi

```
parse roundtrip error: parse error: 2:9: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
```

## tests/vm/valid/left_join.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/left_join_multi.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

## tests/vm/valid/list_assign.mochi

```
parse roundtrip error: parse error: 2:13: unexpected token "="
```

## tests/vm/valid/list_nested_assign.mochi

```
parse roundtrip error: parse error: 2:18: unexpected token "="
```

## tests/vm/valid/list_set_ops.mochi

```
parse roundtrip error: parse error: 2:9: unexpected token "*" (expected ")")
```

## tests/vm/valid/load_yaml.mochi

```
parse roundtrip error: parse error: 1:17: lexer: invalid input text "'T')\nlet K = Typ..."
```

