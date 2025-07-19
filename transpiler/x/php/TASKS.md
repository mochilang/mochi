# PHP Transpiler Tasks

## Progress
- 2025-07-19 04:52 UTC - Initial checklist and support for let statements
- 2025-07-19 05:30 UTC - Added unary negation, list literals, var assignments and helpers

- 2025-07-19 05:48 UTC - Added typed let support and new tests
- 2025-07-19 07:41 +0000 - Added conditional expressions, if statements and typed vars
  enabling len_string, typed_var, if_else, if_then_else and if_then_else_nested tests
- 2025-07-19 09:40 UTC - Removed helper runtime functions and switched to builtin echo/count/strlen
- 2025-07-19 10:39 UTC - Added substring builtin support enabling substring_builtin test
- 2025-07-19 21:29 GMT+7 - Implemented integer division and new builtins (sum,
  avg, str) plus comparison fixes enabling binary_precedence, math_ops,
  string_compare, str_builtin, sum_builtin and avg_builtin tests
