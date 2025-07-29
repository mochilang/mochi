# Scheme AST Conversion

Created: 2025-07-28

This directory provides a very small converter used in the tests.  It parses
`define` forms from the Scheme sources under `tests/transpiler/x/scheme` and
emits stub Mochi declarations.  Only function and variable definitions are
recognised. The generated Mochi files now include a meta header with the
repository version and current time (GMT+7) followed by the original source as a
block comment.

Completed programs: 95/95

## Checklist
- [x] group_by
- [x] group_by_conditional_sum
- [x] group_by_sort
- [x] group_items_iteration
- [x] partial_application
- [x] query_sum_select
- [x] tail_recursion
- [x] var_assignment
- [x] set_assignment
- [x] import_statements
- [x] require_statements

