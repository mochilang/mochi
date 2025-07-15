## Recent Enhancements (2025-07-15 03:26)
- JOB dataset tests now cover q1-q33 with struct literals preserved as dataclasses.
## Recent Enhancements (2025-07-15 02:40)
- Selector inference uses stored element types; `_get` helper removed.
## Recent Enhancements (2025-07-14 12:16)
 - Recompiled all programs and restored dataset checklist.
## Recent Enhancements (2025-07-14 07:13)
- TPCH queries q1-q22 regenerate with shared dataclass names.
## Recent Enhancements (2025-07-14 05:55)
- Auto dataclasses omit `__iter__` and `__eq__`; dataclasses use default equality.
- Named structs no longer generate `__iter__` methods.

## Recent Enhancements (2025-07-14 05:39)
- Regenerated machine outputs and restored README sections for dataset queries.
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
## Recent Enhancements (2025-07-14 01:42)
- List literals with complex elements now use multi-line formatting.
- `cross_join.mochi` output aligns with human reference style.
## Recent Enhancements (2025-07-14 02:10)
- Query results with map literals emit dataclasses and update parent scope.
- `cross_join.mochi` uses attribute access and typed result list.
## Recent Enhancements (2025-07-16 03:21)
- Fixed nil dereference in `let` and `var` handling when no initializer is provided.
- Added `__getitem__` back to auto dataclasses and improved `_sort_key` helper for dataclass arguments.

## Recent Enhancements (2025-07-16 04:10)
- `_Group` now defines `__len__` so groups behave like lists.
- Aggregate helpers emit `len(g)` instead of `len(g.Items)` when counting or averaging.

## Recent Enhancements (2025-07-16 05:00)
- Auto and named dataclasses now implement `__iter__` for easier unpacking.
- Prepared compiler output formatting for TPCH Q1 regeneration.
## Recent Enhancements (2025-07-14 04:33)
- Group selector paths now handle `group` types so `g.key` and `g.Items` use
  attribute access with proper type inference.
## Recent Enhancements (2025-07-14 04:55)
- `_sum` helper now always returns a float, fixing `tpc-h/q1.mochi` equality tests.

## Remaining Work
- [x] Extend dataset query support for `tpc-h` queries beyond `q1`.
- [ ] Improve formatting to match examples in `tests/human/x/python`.
- [ ] Enable type hints for all generated code paths.
- [ ] Reduce helper method emission when not required.
