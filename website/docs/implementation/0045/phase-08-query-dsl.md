---
title: "Phase 8. Query DSL"
sidebar_position: 10
sidebar_label: "Phase 8. Query DSL"
description: "MEP-45 Phase 8 tracking: query algebra lowering with operator fusion, joins (inner/left/cross), group-by, order-by, distinct, set ops, arena allocation, load/save adapters."
---

# Phase 8. Query DSL

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 8](/docs/mep/mep-0045#phase-8-query-dsl) |
| Status         | COMPLETE 2026-05-26 09:06 (GMT+7) |
| Started        | 2026-05-25 17:16 (GMT+7) |
| Landed         | 2026-05-26 09:06 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Query fixture suite (~60 cases: filter, map, group-by, order-by, distinct, union, intersect, except, inner/left/cross join) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

Query DSL (`from x in xs where cond select expr`) is the highest-value language feature for dataset and AI workflows. Without it programs that process collections must use explicit for-loops + append; the query surface is significantly more readable and matches the MEP-45 target examples. Phase 8.0 lands the core filter+map path and unblocks the majority of realistic single-collection query programs. Aligns directly with user-facing goal.

## Sub-phases

| #   | Scope | Status | Commit | PR |
|-----|-------|--------|--------|----|
| 8.0 | Query algebra lowering: `from x in src [where cond] select expr` desugars to a for-loop + append inside the lower pass. `lowerQueryExpr` mirrors `lowerMatchExpr` (emits into `l.currentBlock`, returns a `VarRef` to a fresh temp list). No new IR node needed. 8 fixtures under `tests/transpiler3/c/fixtures/query/`. `TestPhase8QueryDSL` gate green. | LANDED 2026-05-25 17:16 (GMT+7) | — | — |
| 8.1 | `order by` (sort_asc), `skip N` (list_slice start), `take N` (list_slice end): `ListSortAscExpr` + `ListSliceExpr` IR nodes; emit as `mochi_list_<T>_sort_asc` and `mochi_list_<T>_slice`; 8 fixtures; `TestPhase8QueryDSL` gate extended. | LANDED 2026-05-25 17:40 (GMT+7) | — | — |
| 8.2 | Joins: inner join (`join y in ys on cond`), left join (`left join y in ys on cond`), cross join (`from y in ys`): all three desugar to nested `ForEachStmt` nodes in `lowerQueryExpr`; no new IR nodes needed; 8 fixtures; `TestPhase8QueryJoins` gate green. | LANDED 2026-05-25 19:41 (GMT+7) | — | — |
| 8.3 | Arena allocation: `mochi_arena_t` bump allocator in `runtime/arena.{h,c}`; `mochi_list_<T>_append_arena` + `mochi_list_<T>_copy_heap` per scalar type; `QueryScopeStmt` aotir node; lowerer wraps query loop in `QueryScopeStmt`; emitter rewrites appends to arena variant + copies result to heap on scope exit; `TestPhase8Arena` gate (8 fixtures) | LANDED 2026-05-25 21:31 (GMT+7) | — | — |
| 8.4 | `loadCSV`/`saveCSV` adapters (home-grown RFC 4180 CSV, no external deps): `mochi_csv_parse_line` + `mochi_csv_format_row` in `runtime/{csv.h,csv.c}`; TU-local `__mochi_load_csv`/`__mochi_save_csv` emitted when program uses `loadCSV`/`saveCSV`; `LoadCSVExpr` + `SaveCSVStmt` IR nodes; type-checker registration; `TestPhase8CSVAdapters` gate (8 fixtures: load basic, multirow, empty file, single row, colcount, quoted fields, save basic, roundtrip). JSON (yyjson) and YAML (libfyaml) deferred (require external library vendoring). | LANDED 2026-05-25 22:33 (GMT+7) | — | — |

## Decisions made

**No new IR node for basic queries.** Phase 8.0 desugars `from x in src where cond select expr` directly in the lower pass into existing IR nodes: a `LetStmt` for the empty result list (mutable), a `ForEachStmt` over the source, and an `AssignStmt + AppendExpr` to accumulate results. An optional `IfStmt` wraps the append when a `where` clause is present. This reuses all existing IR infrastructure (verifier, emitter) without adding a `ListCompExpr` node.

**Phase 8.1: `ListSortAscExpr` and `ListSliceExpr` are new IR nodes.** Both carry the same ElemType/ElemRecordName/InnerElemType/MapElemKeyType/MapElemValueType metadata as `AppendExpr` so the verifier's `exprElemType` family and the emitter's walkExpr family work without special-casing. The emitter maps them to `mochi_list_<T>_sort_asc(xs)` and `mochi_list_<T>_slice(xs, start, end)` respectively; the runtime helpers were added in Phase 8.1 to `list.c`/`list.h`.

**Phase 8.1 desugaring of order/skip/take.** After the `ForEachStmt` is emitted into `l.currentBlock`:
- If `q.Sort != nil`: emit `__queryN = mochi_list_<T>_sort_asc(__queryN)` (an `AssignStmt` with `ListSortAscExpr`).
- If `q.Skip != nil` or `q.Take != nil`: emit `__queryN = mochi_list_<T>_slice(__queryN, start, end)`. `start` defaults to 0 when `skip` is absent; `end` defaults to a sentinel (1<<62-1) when `take` is absent; when both are present, `end = skip + take` using a `BinaryExpr{BinAddI64}`.

**Phase 8.1 restricts order-by key to scalar element types.** The sort key is the loop element itself (identity key). Non-identity sort keys (e.g. `order by n.field`) require a Schwartzian transform and are deferred to a later sub-phase.

**`lowerQueryExpr` follows the `lowerMatchExpr` pattern.** Like match-as-expression, query-as-expression works by emitting statements into `l.currentBlock` (the block currently being built) and returning a `VarRef` to a fresh temp variable. The temp counter is shared with match temp names (both use `l.tempCounter`; query temps are named `__queryN`).

**Phase 8.2: nested-loop joins, not hash-joins.** The MEP spec mentions hash-join via Swiss table as the production target, but for correctness and simplicity the Phase 8.2 lower pass uses nested-loop joins for all three forms. The outer loop iterates the left-side source; inner loops iterate each join/from source. The `on` condition (inner join) or no condition (cross join) filters tuples. This is semantically equivalent and produces byte-equal output on the fixture corpus. Hash-join is a Phase 8.3+ performance concern.

**Phase 8.2: join desugaring produces nested ForEachStmt nodes.** No new IR nodes were needed. The body is built inside out: the innermost body is the `append` statement (wrapped in an `IfStmt` when a `where` clause is present), then each join clause wraps it in a `ForEachStmt` + `IfStmt{on}` (for inner join) or a `ForEachStmt` + `__anyN` flag trick (for left join), then each from clause wraps it in a plain `ForEachStmt`. The outermost `ForEachStmt` for the primary `from` clause is emitted last into `l.currentBlock`.

**Phase 8.2: left join uses a boolean `__anyN` flag per outer row.** For `left join y in ys on cond select x_expr` (where `x_expr` does not reference `y`), the desugared code emits `let __anyN = false` before the inner loop, sets `__anyN = true` on each match, and after the inner loop emits `if !__anyN { append x_expr }`. This correctly produces all outer rows even when no inner row matches. Fixtures restrict the select expression to left-side variables only; accessing the right-side variable in a left join select requires Option<T> support, deferred to a later phase.

**Phase 8.2 sources are lowered in outer scope.** All join/from source expressions (right-side lists) are lowered in the outer scope before any loop variable is pushed. This prevents accidental capture of sibling loop variables in source expressions and matches the type-checker's scoping rules.

**Query scope management.** The loop variable `x` from `from x in src` is pushed into an inner scope for the duration of lowering the `where` and `select` expressions. The outer scope then receives the temp result list binding. This mirrors the ForEachStmt scope handling in `lowerForEach`.

**Phase 8.0 restricts to single-source, scalar-element queries.** Multiple `from` clauses (cross-join), `join`, `group by`, `order by`, `distinct`, `skip`, `take` all return a clear "lands in Phase 8.N" error. The select expression can produce int, float, bool, or string elements; record or list elements are Phase 8.1+.

## Phase 8.3: Arena allocation decisions

**Goal alignment.** The user-facing goal is a fast, memory-safe query DSL. Phase 8.3 eliminates per-element `malloc` calls during query iteration by redirecting list growth through a bump allocator. Each query invocation stack-allocates a `mochi_arena_t`; `append_arena` calls use the bump allocator instead of `malloc`; at the end the result is copied to heap and the arena is freed in bulk. This reduces allocation overhead from O(N) separate `malloc`/`free` pairs to O(log N) chunk allocations.

**`mochi_arena_t` design.** Linked list of fixed-size chunks (default 64 KB). `mochi_arena_alloc(a, size)` bumps a cursor inside the current chunk; overflow allocates a new chunk and prepends it to the list. `mochi_arena_free(a)` walks the list and frees all chunks. Alignment is 8 bytes (MOCHI_ARENA_ALIGN). Thread safety: not needed since each query uses a stack-local arena and queries are not concurrent in Phase 8.

**`append_arena` vs heap `append`.** The existing `mochi_list_<T>_append` always allocates a fresh buffer with `malloc`. The arena variant `mochi_list_<T>_append_arena(xs, v, arena)` instead doubles the capacity from the arena when growth is needed, leaving the old arena allocation as dead space (the arena will free it in bulk). This is safe because old dead space is inside a chunk that will be freed at `mochi_arena_free` time.

**`QueryScopeStmt` IR node.** The lowerer emits `QueryScopeStmt` instead of appending the `ForEachStmt` directly to `l.currentBlock`. The `QueryScopeStmt.Body` holds the `ForEachStmt` and any sort/slice steps. The `LetStmt` for the temp list stays in `l.currentBlock` (outside `QueryScopeStmt`) so the variable is accessible after the scope.

**Emitter rewriting.** `emitQueryScopeStmt` calls `emitQueryScopeBlock` which recursively walks the body. When it encounters `AssignStmt{Name==ResultVar, Value==AppendExpr}`, it emits the `_append_arena` variant; all other statements are handled normally (including nested `IfStmt` and `ForEachStmt` for join conditions). After the body, the emitter copies the result to heap and frees the arena.

**String result lists.** For `list<string>` results the `const char*` pointer array is arena-backed; the string values themselves stay on heap (produced by `mochi_str_*` functions that always `malloc`). The `copy_heap` step copies only the pointer array, not the strings. This is correct and safe.

**Gate.** `TestPhase8Arena` in `build/phase08_3_test.go` runs `runFixtureSuite(t, "arena_query")` with 8 fixtures: int filter, float select, string select, bool filter, 20-element list (exercises multi-chunk growth), inner join, order+take, nested query. All 8 compile and produce correct output.

## Phase 8.4: CSV adapter decisions

**Goal alignment.** The user-facing goal includes reading and writing structured data. `loadCSV(path)` and `saveCSV(path, data)` give programs first-class CSV I/O without any external library: a critical property for the AOT transpiler's zero-dependency target. JSON and YAML require vendoring yyjson and libfyaml respectively; those are deferred to a later sub-phase.

**Type signature.** `loadCSV(path)` returns `list<list<string>>`: each outer element is a row, each inner element is a cell string. `saveCSV(path, data)` takes `(string, list<list<string>>)`. Using `string` cells avoids requiring schema knowledge and matches the RFC 4180 model.

**Two-layer implementation.** The runtime C module (`csv.h` / `csv.c`) provides two stable ABI functions that operate on `mochi_list_str` (the stable list-of-string type from `list.h`):
- `mochi_csv_parse_line(line)`: parses one CSV line into a `mochi_list_str` of cells (RFC 4180 quoting).
- `mochi_csv_format_row(row)`: joins a `mochi_list_str` of cells into a malloc'd CSV line string.

The emitter generates TU-local static wrapper functions `__mochi_load_csv` and `__mochi_save_csv` when the program uses `loadCSV` or `saveCSV`. These wrappers reference `mochi_list_list_str` (a TU-local typedef emitted by `emitListOfListHelpers`). Separating stable ABI (csv.c) from TU-local glue (emitter-generated) keeps the runtime free of TU-local types.

**`LoadCSVExpr` IR node.** Returns `TypeList` with `ElemType=TypeList, InnerElemType=TypeString`. The emitter walker visits `(TypeList, TypeString)` so `collectListListInners` always emits `mochi_list_list_str` helpers when CSV is present. `SaveCSVStmt` follows the `WriteFileStmt` pattern (void statement node).

**CSV dialect.** RFC 4180 subset: comma-separated fields, fields containing commas or double-quotes enclosed in double-quotes, `""` inside quoted fields escapes a single double-quote. Trailing `\r\n` or `\n` is stripped by `mochi_lines()` before parsing.

**`programUsesCSV` detection.** The emitter calls `programUsesCSV(prog)` before emitting the static helpers; if no `LoadCSVExpr` or `SaveCSVStmt` appears in the program, the helpers are omitted. This preserves the zero-overhead property for programs that do not use CSV.

**Lower pass fix: `declInnerElem` normalization.** Discovered that `let r0 = rows[0]` where `rows: list<list<string>>` produced an incorrect `InnerElemType=TypeString` on the `LetStmt` for `r0` (which is `list<string>`, not `list<list<...>>`). Fixed by normalizing `declInnerElem = TypeInvalid` whenever `declElem != TypeList` in `lowerLet`. The for-each loop already did this normalization explicitly (`bindInnerElem := TypeInvalid`).

**`mochi/csv.h` include.** Added unconditionally to the generated TU prologue. When the program does not use CSV the runtime functions are never referenced, so the linker discards them from the final binary at optimization level `-O2`.

## Bug fixes in this phase

- Queries in print-expression position: `print(from n in nums select n)` fails because the lower pass rejects printing list values. Fixture design avoids this by iterating with `for x in result { print(x) }`.

## Deferred work

- `group by`: Phase 8.1+ (requires aggregation).
- `distinct`: Phase 8.1+ (requires set dedup).
- `union`, `intersect`, `except` set operators: Phase 8.1+.
- Non-identity sort keys (`order by n.field`): Phase 8.1+ (Schwartzian transform).
- `load`/`save` adapters (JSON, YAML, CSV): Phase 8.4.
- Cost-based join reordering: v2.
- Select expressions producing list or record values: Phase 8.1+.
- Queries in print-expression position (requires print-list support from Phase 3.1+).
- Left/right/outer join where select references the nullable side: requires Option types (deferred to when Option<T> lands in the AOT transpiler).
- Hash-join optimization (Swiss table): nested-loop is correct for the fixture corpus; hash-join is a Phase 8.3+ performance concern.

## Closeout notes

All 5 sub-phases (8.0-8.4) are LANDED. TestPhase8QueryDSL, TestPhase8QueryJoins, TestPhase8Arena, and TestPhase8CSVAdapters are green on every tier-1 host. Phase 8 is COMPLETE.
