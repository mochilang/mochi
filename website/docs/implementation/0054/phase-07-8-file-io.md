---
title: "Phase 7.8. File I/O"
sidebar_position: 20
sidebar_label: "Phase 7.8. File I/O"
description: "MEP-54 Phase 7.8, writeFile / appendFile / readFile / lines via os.* + mochi helpers."
---

# Phase 7.8. File I/O

| Field          | Value |
|----------------|-------|
| MEP            | MEP-54 |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking PR    | [#22583](https://github.com/mochilang/mochi/pull/22583) |
| Commit         | 492a45a1c9 |

## Gate

8 fixtures cover the I/O surface: `file_append_basic`, `file_write_read_basic`, `file_write_overwrite`, `file_write_read_newlines`, `file_read_long`, `lines_basic`, `lines_empty_file`, `lines_no_trailing_newline`. 230 transpiler3/go fixtures green.

## Lowering decisions

`WriteFileStmt` dispatches in `stmt.go` to `os.WriteFile(path, []byte(content), 0644)`. The mode `0644` matches the C runtime default and is the canonical Go idiom for user-rw / group-r / other-r.

`AppendFileStmt` dispatches to `mochiAppendFile`, a helper that opens with `O_APPEND|O_CREATE|O_WRONLY`, writes the content, closes the file, and ignores errors. Go's stdlib has no one-liner for append-create-and-write; the helper packages the three syscalls and matches the C runtime's silent-on-error behaviour (which Mochi's semantics promise).

`ReadFileExpr` lowers to `mochiReadFile`, which calls `os.ReadFile(path)` and returns the string content (or `""` on error). The empty-string-on-error contract matches the C runtime's `mochi_read_file`.

`LinesExpr` lowers to `mochiLines`, which calls `os.ReadFile(path)`, splits on `'\n'`, and drops a trailing empty token so a file ending with a newline produces N lines, not N+1. The trailing-newline drop matches the C runtime's behaviour for `mochi_file_lines`, which the JVM and BEAM transpilers also mirror.

`addImport("os")` is registered for the helpers; the helpers themselves are emitted via `addHelper` into the `emittedHelpers` block.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/go/lower/stmt.go` | `WriteFileStmt`, `AppendFileStmt` dispatch |
| `transpiler3/go/lower/expr.go` | `ReadFileExpr`, `LinesExpr` dispatch |
| `transpiler3/go/lower/lower.go` | `mochiAppendFile`, `mochiReadFile`, `mochiLines` helper texts |
| `tests/transpiler3/go/fixtures/file_*/`, `lines_*/` | 8 fixtures |

## Test set

- 8 `TestPhase1Hello/file_*`, `lines_*` subtests.

## Closeout notes

The "ignore errors" contract for file I/O matches every other Mochi transpiler's behaviour; surfacing errors here would be a semantic divergence that the upstream type-checker is not prepared for. The trailing-newline drop in `mochiLines` is the kind of detail that's easy to miss in a fresh-write helper: the test `lines_no_trailing_newline` exercises both shapes (with and without the trailing newline) so the contract is pinned.
