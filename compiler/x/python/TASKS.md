## Recent Enhancements (2025-07-16 18:00)
- Removed generated code comparisons from tests to rely solely on runtime output.
- Deprecated `.py.out` files in `tests/compiler/py`.
## Recent Enhancements (2025-07-16 18:30)
- Added `UNDEFINED` sentinel and updated `_fmt` so uninitialized values print `undefined`.
- Dataclasses now define `__contains__` for membership checks.
- Map literals with string keys remain dictionaries, fixing `len_map`.
- `_save` JSON output no longer sorts keys and golden files updated.
- `_load` resolves relative paths using `MOCHI_ROOT`; VM tests set this env var.
- Struct arguments are copied using `dataclasses.replace` to preserve pass-by-value semantics.
## Recent Enhancements (2025-07-16 19:00)
- VM golden tests refreshed for `tests/vm/valid`; generated Python code lives under `tests/machine/x/python` and matches runtime output.
- `_fmt` returns `null` for uninitialized values and integer floats print without decimals; `_save` now sorts keys for stable JSON.
- `print` calls ignore empty arguments to avoid trailing spaces.
## Recent Enhancements (2025-07-16 15:45)
- Added golden tests for `tests/vm/valid` to verify compiled Python output.
- `_save` helper now dumps JSON with sorted keys and compact separators, fixing `save_jsonl_stdout` mismatch.

## Recent Enhancements (2025-07-16 17:02)
- Added `vm_golden_test.go` running programs under `tests/vm/valid` and storing
  outputs in `tests/machine/x/python`.
- Introduced `_fmt` helper and improved `print` handling for lists and floats.
- Removed legacy `TestCompilePrograms` and helpers.

## Recent Enhancements (2025-07-15 05:54)
- Updated _sort_key to handle nested lists and tuples
- Regenerated TPCDS q1-q3 Python outputs

## Recent Enhancements (2025-07-15 08:19)
- Fixed group-by code generation for single-value rows so TPCH q1-q22 compile
  and run without indexing errors.
- Regenerated TPCH Python outputs and verified golden tests pass.

## Recent Enhancements (2025-07-15 05:05)
- Regenerated TPCDS q1-q2 Python outputs without type hints; tests pass.

## Recent Enhancements (2025-07-15 04:51)
- Grouped query fields correctly map to tuple aliases, enabling TPCDS q1-q2 regeneration.

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
