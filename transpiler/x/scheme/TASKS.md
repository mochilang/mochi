## Progress (2025-07-20 11:38 +0700)
- Enhanced static type selection for `len`, `append` and indexing
- Removed unnecessary runtime checks in generated code
- Updated README checklist and outputs

## Progress (2025-07-20 10:18 +0700)
- Removed bool to int conversion; booleans now print as `#t`/`#f`.
- Added typed default values for `int`, `bool` and `string`.
- Updated golden files and README checklist (34/100 passing).

## Progress (2025-07-20 09:22 +0700)
- Generated Scheme for 32/100 programs
- Updated README checklist and outputs

## Progress (2025-07-20 08:31 GMT+7)
- Fixed default value for typed variables to `nil`
- Regenerated Scheme outputs (24/100 passing)

## Progress (2025-07-19 19:43 GMT+7)
- Added support for membership operator and string `contains` method
- Implemented string-aware `len`, `append`, `substring`, `min`, `max`, and modulo
- Generated new Scheme outputs for golden tests (32/100 passing)

## Progress (2025-07-19 18:42 GMT+7)

# Progress

Last updated: 2025-07-19 07:12 +0000

- 2025-07-19: initial print-only implementation
- 2025-07-19: support let statements and arithmetic; updated tests
- 2025-07-19: added comparison operators and typed lets; updated golden tests
- 2025-07-19: added unary negation and string operations; more golden tests
- 2025-07-19: implemented `var` declarations and assignments; generated tests
