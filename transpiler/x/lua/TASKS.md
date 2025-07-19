# Lua Transpiler Tasks

## Progress (2025-07-19 12:38 +0700)
- Added string concatenation using `..` when operands are strings.
- Added support for comparison expressions.
- New golden tests `string_concat` and `string_compare`.

## Progress (2025-07-19 05:23)
- Moved documentation under `transpiler/x/lua`
- Added support for unary negation and new golden test `unary_neg`

## Progress (2025-07-19 11:50)
- Initial Lua transpiler supporting `print` with a single string literal.

## Progress (2025-07-19 05:02)
- Added integer literals and basic arithmetic support.
- New golden test `binary_precedence` ensures operator precedence works.
