# Python Compiler Tasks

## Recent Enhancements (2025-07-14 02:43)
- Query result structs are now named using the destination variable for improved readability.

## Recent Enhancements (2025-07-13 05:14)
- Added nested function generation for returned arrow functions.
- Added versioned header comments to generated Python files.
- Verified `tpc-h/q1.mochi` compiles and runs correctly.
## Recent Enhancements (2025-07-13 05:18)
- Dataclasses now use concrete field types and omit `__getitem__` helpers.
- Result records are emitted as dataclasses instead of plain dicts.
- Print calls with multiple arguments emit a single f-string for clarity.
## Recent Enhancements (2025-07-13 19:29)
- Auto-struct lists update environment types so dataset queries use attribute access.
- `tpc-h/q1.mochi` runs with generated dataclasses using dot notation.
## Recent Enhancements (2025-07-14 01:31)
- Anonymous struct literals now emit dataclasses.
- `let` and `var` statements store named types for later attribute access.
## Recent Enhancements (2025-07-14 01:42)
- List literals with complex elements now use multi-line formatting.
- `cross_join.mochi` output aligns with human reference style.
## Recent Enhancements (2025-07-14 02:10)
- Query results with map literals emit dataclasses and update parent scope.
- `cross_join.mochi` uses attribute access and typed result list.

## Remaining Work
- [x] Extend dataset query support for `tpc-h` queries beyond `q1`.
- [ ] Improve formatting to match examples in `tests/human/x/python`.
- [ ] Enable type hints for all generated code paths.
