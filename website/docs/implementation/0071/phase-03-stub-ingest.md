---
title: "Phase 3. PEP 561 stub ingest"
sidebar_position: 5
sidebar_label: "Phase 3. stub ingest"
description: "MEP-71 Phase 3 lands the 4-tier PEP 561 stub discovery (inline / sibling-stubs / typeshed / stubgen), the typeshed pin, and a focused .pyi reader."
---

# Phase 3. PEP 561 stub ingest

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-71 §Phases](/docs/mep/mep-0071#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 23:12 (GMT+7) |
| Landed         | 2026-05-29 23:22 (GMT+7) |
| Tracking issue | (filled by automation) |
| Tracking PR    | (filled by automation) |
| Commit         | (filled by automation) |

## Gate

`TestPhase3StubIngest` in `package3/python/stubs/phase03_test.go` with subtests:

- `inline_tier_wins`. Constructs a fake site-packages with both a `py.typed`-marked package and a parallel `<name>-stubs` directory. Resolves to TierInline, confirming the PEP 561 priority order is honoured.
- `sibling_stubs_tier`. Constructs a site-packages with only `<name>-stubs`. Resolves to TierSiblingStubs.
- `typeshed_tier`. Constructs a fake typeshed checkout under `stdlib/pkg/__init__.pyi`. Resolves to TierTypeshed.
- `stubgen_fallback`. Drops to the stubgen tier via a `FakeStubgen` that writes a fixed body to disk. Asserts `Partial = true` and round-trips the generated `.pyi` through `ParsePYI` to confirm the end-to-end pipeline produces a parseable `ModuleSurface`.
- `pyi_round_trip`. Parses a representative `.pyi` exercising imports / functions / classes (with method + field) / aliases / constants, and asserts the structured surface matches.

The package-level coverage:

- `package3/python/stubs/discovery_test.go`. Tier string rendering (6 cases including the unknown / out-of-range sentinel), empty-name rejection, inline-full vs inline-partial via `py.typed` body sniff, sibling stubs discovery, inline-beats-sibling, sibling-beats-typeshed, typeshed-beats-stubgen, stubgen fallback path, stubgen-disabled rejection, plain not-found error, hyphenated PEP 503 name mapping to underscore module name, walkStubFiles sort order across nested directories, `.py` fallback only when no `.pyi` twin exists, `readPartialMarker` over 6 body shapes including whitespace tolerance.
- `package3/python/stubs/typeshed_test.go`. `NewTypeshed` rejects empty root, missing path, regular file, and non-typeshed directory; accepts stdlib-only and stubs-only checkouts; `Lookup` covers stdlib directory, stdlib `.pyi` file, third-party nested `stubs/<name>/<module>`, third-party flat `stubs/<name>`, and miss; `moduleFromName` over 6 PEP 503 names (uppercase, hyphen-to-underscore, empty); `dirExists` over directory / missing / regular-file.
- `package3/python/stubs/stubgen_test.go`. `FakeStubgen.Generate` writes the right body to the right path and produces a `StubSource` with `Tier = TierStubgen`, `Partial = true`; `FakeStubgen` rejects empty `CacheDir`; module-vs-name aliasing (e.g. `Flask-SQLAlchemy` -> `flask_sqlalchemy.pyi`); `NewExecStubgen` defaults (Python lookup deferred, 60s timeout); `ExecStubgen` rejects empty `CacheDir` and surfaces a wrapped error when the interpreter does not exist.
- `package3/python/stubs/pyi_test.go`. Empty body, `import X` / `import X as Y` / `from X import Y, Z as W` / parenthesised multi-line from-imports, classes with field + method + base, Protocol / TypedDict / TypedDict-with-options, `@dataclass` and `@dataclasses.dataclass`, sync + async functions, default values, positional-only (`/`), keyword-only (`*`), `*args` / `**kwargs` kind tracking, method decorators (`@property` / `@staticmethod` / `@overload`), type aliases, `TypeVar('T')`, scalar constants, constants with default values, comment stripping (top-level / trailing / value-side / `#` inside a string), backslash continuation, parenthesised continuation, unbalanced bracket rejection, BOM tolerance, `==` is not split as `=` in aliases, unknown top-level constructs silently skipped, plain-identifier sniffing (10 cases), top-level colon finder (6 cases including bracket / string content), top-level eq finder (5 cases rejecting `==`, `>=` and friends), top-level splitter respecting brackets + strings, dataclass round-trip covering imports + decorated class + method with forward-reference return type, tabs as indent.

## Lowering decisions

Phase 3 owns four sub-systems:

- `package3/python/stubs/discovery.go`. `Discovery{SitePackages, Typeshed, Stubgen, AllowStubgen}` runs the 4-tier search. The PEP 503 distribution name is normalised to the import module name via `name -> strings.ReplaceAll(name, "-", "_")` (a stricter normalisation is reserved for phase 4 once the type mapper sees the surface). `StubSource{Package, Tier, RootDir, Files, Partial}` is the cross-tier handoff. `walkStubFiles` returns sorted relative paths under the root, preferring `.pyi` and only including `.py` when no `.pyi` twin shadows it. `readPartialMarker` sniffs PEP 561 `py.typed` for the literal token "partial" (other bodies are tolerated and treated as full).
- `package3/python/stubs/typeshed.go`. `Typeshed{Root, Commit}` wraps a pinned local checkout. `NewTypeshed` validates the canonical layout (at least one of `stdlib/` or `stubs/`). `Lookup` searches stubs/<name>/<module>/ first (the typeshed convention), falls back to stubs/<name>/ (for entries where the stub directory matches the PEP 503 name exactly), then stdlib/<module>/, then stdlib/<module>.pyi. The pin commit is recorded but not enforced at lookup time; the workspace orchestrator (phase 8) ensures the checkout is at the pinned commit before discovery runs.
- `package3/python/stubs/stubgen.go`. `Stubgen` is an interface so tests can substitute. `ExecStubgen` shells out to `<python> -m mypy.stubgen --package <module> --output <cache>/<name> --quiet` with a 60-second default timeout. `FakeStubgen` writes a fixed body to `<cache>/<name>/<module>.pyi`; both implementations stamp `Partial = true` on every output so phase 4 will refuse to emit wrappers for synthesised surfaces unless the build sets `--allow-partial`.
- `package3/python/stubs/pyi.go`. The `.pyi` parser is intentionally not a full Python parser. `ParsePYI` walks logical lines (joining backslash and parenthesised continuations), strips comments outside of strings (including triple-quoted), tracks bracket depth and rejects mismatched brackets, and dispatches on the leading token. It extracts the surface phase 4 + 5 consume: imports (`ImportDecl` / `ImportedName`), classes (`ClassDecl` with `IsProtocol` / `IsTypedDict` / `IsDataclass` and inline `Methods` + `Fields`), functions (`FuncDecl` with `IsAsync`, decorators, params, return type), parameters (`ParamDecl` with PEP 570 / PEP 3102 `ParamKind` tracking `/` and `*` separators plus `*args` / `**kwargs`), aliases (`Name = Expr`), constants (`Name: Type [= value]`). Type expressions are stored as raw strings; phase 4 will parse them against the closed type-mapping table.

The 4-tier order matches PEP 561 §"Stub-Only Packages":

1. Inline stubs ship with the package and announce themselves via `py.typed`. If the file contents are the literal token "partial", the source is tagged `Partial = true` and phase 4 will refuse to wrap symbols without explicit annotations.
2. Sibling stubs distributed as `<name>-stubs` (PEP 561 §"Stub-only packages") take precedence over typeshed because they are tied to a specific package version.
3. Typeshed is a centralised, community-maintained source. The bridge pins a commit so type information is reproducible across machines.
4. Stubgen is the last-resort synthesiser. The bridge does not run mypy stubgen at discovery time without `AllowStubgen = true`; phase 8 sets that flag based on the build's policy.

The deliberate "structural .pyi parser" decision is documented at the top of `package3/python/stubs/doc.go`. We avoid pulling in a full Python parser (and there is no Go-native CPython AST library that we trust in-tree). The reader is ~720 lines and handles the constructs phase 4 + 5 actually use; anything else is silently skipped so we degrade gracefully on exotic stubs (TypeVar tuples, ParamSpec, decorated module-level assignments) rather than blowing up the build.

## Files changed

| File | Purpose |
|------|---------|
| `package3/python/stubs/doc.go` | Package doc (4-tier discovery + scope of the .pyi reader) |
| `package3/python/stubs/discovery.go` | `Tier`, `StubSource`, `Discovery`, `walkStubFiles`, `readPartialMarker` |
| `package3/python/stubs/typeshed.go` | `Typeshed`, `NewTypeshed`, `Lookup`, `moduleFromName`, `dirExists` |
| `package3/python/stubs/stubgen.go` | `Stubgen` interface, `ExecStubgen`, `FakeStubgen` |
| `package3/python/stubs/pyi.go` | `ModuleSurface`, `ParsePYI`, logical-line + comment-strip + bracket-track helpers |
| `package3/python/stubs/discovery_test.go` | Discovery + tier-priority + walk + partial-marker tests |
| `package3/python/stubs/typeshed_test.go` | Typeshed constructor + lookup matrix + name-normalisation tests |
| `package3/python/stubs/stubgen_test.go` | FakeStubgen + ExecStubgen tests (no Python required) |
| `package3/python/stubs/pyi_test.go` | `.pyi` parser tests (imports / classes / funcs / aliases / constants + helper coverage) |
| `package3/python/stubs/phase03_test.go` | `TestPhase3StubIngest` sentinel with 5 subtests |

## Test set

- `TestPhase3StubIngest/inline_tier_wins`
- `TestPhase3StubIngest/sibling_stubs_tier`
- `TestPhase3StubIngest/typeshed_tier`
- `TestPhase3StubIngest/stubgen_fallback`
- `TestPhase3StubIngest/pyi_round_trip`
- All `package3/python/stubs/...` unit tests.

## Closeout notes

Phase 3 deliberately does not implement type mapping. The `.pyi` reader hands phase 4 a `ModuleSurface` carrying raw type-expression strings; phase 4 owns the closed table that decides what becomes `int` vs `int64` vs `BigInt`, what Optional[T] becomes vs `T | None`, and where Protocol classes lower into Mochi interfaces. Keeping the structural reader simple means we can grow the type table without churning the parser.

The `Partial` flag is the load-bearing handoff to phase 4. Stubgen output is partial by definition; PEP 561 `partial` py.typed is partial by author intent. Phase 4's wrapper emitter respects this flag and refuses to wrap symbols without explicit annotations when `Partial == true` and the build did not opt into `--allow-partial`. This is the deterministic safety net that prevents the bridge from inventing types when the upstream package has not committed to them.

No CPython runtime, no mypy install, and no typeshed checkout is required for any test in this phase. `FakeStubgen` substitutes for the subprocess; `t.TempDir()` constructs site-packages and typeshed layouts in memory.
