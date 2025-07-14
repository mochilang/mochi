# Python Compiler Tasks

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

## Remaining Work
- [x] Extend dataset query support for `tpc-h` queries beyond `q1`.
- [ ] Improve formatting to match examples in `tests/human/x/python`.
- [ ] Enable type hints for all generated code paths.
