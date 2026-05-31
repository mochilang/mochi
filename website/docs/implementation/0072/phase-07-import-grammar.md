---
title: "Phase 7. import-ts grammar"
sidebar_position: 9
sidebar_label: "Phase 7. import-ts"
description: "MEP-72 Phase 7: `import ts \"<pkg>@<semver>\" as <alias>` grammar + parser. Recognises npm: and jsr: prefix selectors; binds the alias into the Mochi-side namespace; flows the binding through to phase 6's extern emitter."
---

# Phase 7. import-ts grammar

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase7ImportTs` in `parser/import_ts_test.go`: subtests `bare_npm`, `prefixed_npm`, `prefixed_jsr`, `with_alias`, `with_specifier_path`, `with_semver_range`, `lockfile_pinned_only`, `parse_errors`. The first six parse representative `import ts "..." as <alias>` forms and assert the AST contains the expected fields. The seventh asserts that a non-pinned semver-range fails the parser unless `mochi.lock` is present with a pinned version. The eighth tests malformed inputs (missing semver, invalid alias, bare path without prefix, prefix without scope) and asserts each produces a clear diagnostic.

## Lowering decisions

Grammar:

```ebnf
import_ts_stmt    = "import" "ts" ts_specifier "as" identifier
ts_specifier      = '"' ts_specifier_body '"'
ts_specifier_body = registry_prefix? package_ref ("@" semver)? path?
registry_prefix   = "npm:" | "jsr:"
package_ref       = scoped_pkg | unscoped_pkg
scoped_pkg        = "@" identifier "/" identifier
unscoped_pkg      = identifier
semver            = semver_pinned | semver_range
semver_pinned     = digit+ "." digit+ "." digit+ ("-" prerelease)?
semver_range      = caret_range | tilde_range | hyphen_range | "latest" | "*"
path              = "/" path_segment ("/" path_segment)*
```

Examples:

- `import ts "zod@3.22.4" as z` — npm package, pinned version (npm is the default registry).
- `import ts "npm:zod@^3.22" as z` — npm package, explicit prefix, caret range.
- `import ts "jsr:@std/encoding@^1" as enc` — JSR package, scoped, caret range.
- `import ts "jsr:@hono/hono@^4" as hono` — JSR package, scoped.
- `import ts "lodash@^4.17/fp/curry" as curry` — npm package, subpath import.
- `import ts "drizzle-orm@latest" as db` — npm package, latest tag.

The parser recognises the import statement at the top of a Mochi file (same position as `import` for Mochi modules). The AST node `ImportTsStmt` carries `{registry, scope, name, semver, subpath, alias, position}`.

A semver range (not a pinned version) is allowed only when `mochi.lock` contains a pinned resolution for the same `(registry, scope, name)` triple. Lock-mode `--check` and lock-mode `--regenerate` are distinguished: `--check` fails on ranges without a lock entry; `--regenerate` resolves the range against the registry and writes a pinned entry.

The `alias` identifier follows Mochi's identifier rules (start with letter or underscore, ASCII alphanumeric plus underscore, no shadow of built-in names). Two `import ts` statements in the same file must use distinct aliases (a duplicate-alias diagnostic catches the conflict at parse time).

The phase wires the parser into MEP-52 phase 12's existing FFI surface so that the alias is in scope for the rest of the file. References to alias-namespaced functions (`z.string()`, `hono.serve()`) are bound to the corresponding `extern fn` declarations from phase 6's emitted shim.

## Files changed

| File | Purpose |
|------|---------|
| `parser/import_ts.go` | parser entry for `import ts` statements |
| `parser/import_ts_test.go` | `TestPhase7ImportTs` sentinel |
| `ast/import_ts.go` | `ImportTsStmt` AST node + walker hooks |
| `types/import_ts_binder.go` | alias binding into the file-level symbol table |

## Test set

8 subtests as listed in the Gate section.

## Cross-references

- [MEP-72 §4 Surface syntax](/docs/mep/mep-0072#4-surface-syntax-import-ts--as-alias) — the normative grammar.
- [Research note 01 §1 The import-ts statement form](/docs/research/0072/01-language-surface#1-the-import-ts-statement-form) — the design rationale.
- [MEP-74 phase 8 import-go grammar](/docs/implementation/0074/phase-08-import-grammar) — the sister Go-side grammar phase.
