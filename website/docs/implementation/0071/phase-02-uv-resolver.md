---
title: "Phase 2. uv resolver bridge"
sidebar_position: 4
sidebar_label: "Phase 2. uv resolver"
description: "MEP-71 Phase 2 lands the uv subprocess wrapper, a minimal TOML reader, the uv.lock decoder, and PEP 751 pylock.toml round-trip."
---

# Phase 2. uv resolver bridge

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-71 §Phases](/docs/mep/mep-0071#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 23:02 (GMT+7) |
| Landed         | 2026-05-29 23:09 (GMT+7) |
| Tracking issue | (filled by automation) |
| Tracking PR    | (filled by automation) |
| Commit         | `a38cb023` |

## Gate

`TestPhase2UvResolver` in `package3/python/uv/phase02_test.go` with subtests:

- `parse_uv_lock`. Decodes the sampled uv.lock fixture covering registry / git / path sources, dependencies with extras, multiple wheels per package, and a sdist entry. Asserts top-level version, requires-python, and the four-package count.
- `uv_lock_to_pylock`. Converts the decoded uv.lock into a PEP 751 PyLock and renders it. Asserts the canonical header, `lock-version = "1.0"`, and `created-by = "mochi-python-bridge"` appear.
- `pylock_round_trip`. Parses pylock.toml, re-renders, re-parses. The package count is identical.
- `runner_options`. `LockOptions.BuildLockArgs` produces the expected CLI flags for python-version / resolution / index-url / extra-index-url / no-build.
- `md5_split_hash_rejected`. SplitHash returns the raw algorithm faithfully so phase 1's verifier can refuse md5 at the policy layer rather than silently in the parser.

The package-level coverage:

- `package3/python/toml/parser_test.go`. Scalar types (string / int / float / bool / literal string), escape handling (`\t`, `\n`, `\"`, `\\`, `\u00XX`), nested tables (`[server.tls]`), array-of-tables (`[[package]]` + `[[package.wheels]]`), inline tables (`{x = 1, y = 2}`), scalar arrays, mixed empty / trailing-comma arrays, array-of-inline-tables, uv.lock-shaped fixture, comments (header / trailing / between / at table header), quoted keys, underscores in numbers, empty strings. Rejects unterminated strings, newline-in-string, multiline `"""..."""` and `'''...'''`, non-decimal `0x` ints, dotted LHS keys, duplicate keys at top-level and in a table, unterminated arrays / inline tables, missing comma in arrays.
- `package3/python/toml/decoder_test.go`. Typed accessors (String / Int / Bool / Table / TableArray / StringArray / Keys / StringRequired) including absent-key and wrong-type error paths.
- `package3/python/uv/lock_test.go`. uv.lock decode (sampled fixture) covering all three source kinds, dependency extras, wheel + sdist arrays, PackagesByName + SortedPackageNames, SplitHash table (7 cases including absent colon, leading colon, trailing colon, uppercase normalisation), missing-name error.
- `package3/python/uv/pylock_test.go`. PEP 751 envelope, dependencies array, wheel hash table with multiple algos (blake3 + sha256 -> alphabetical render order), sdist sub-table, missing-hashes rejected, round-trip, deterministic render across 6 iterations, canonical header presence, FromLockfile conversion (wheel filename derivation from URL, sdist preservation).
- `package3/python/uv/runner_test.go`. LockOptions table over 9 combinations including defaults / highest / lowest / lowest-direct / explicit python-version / index-url / multiple extra-indexes / no-build / combined. `Export` exercise via a fake Runner verifies workDir + argv shape. `Locate` returns either a path or an explicit not-found error, never `("", nil)`.

## Lowering decisions

Phase 2 splits into three sub-packages so the bridge can mock each independently in phase 8 and phase 9:

- `package3/python/toml/`. A hand-rolled minimal TOML reader. The bridge does not introduce a TOML library dependency. The reader supports the subset required by uv.lock and pylock.toml: scalar types (string, int64, float64, bool), tables, arrays of tables, inline tables, scalar arrays, array-of-inline-tables, basic + literal strings, comments. Multiline strings, datetimes, non-decimal int literals (0x / 0o / 0b), and dotted LHS keys are rejected with a clear error. The output is a `map[string]any` tree where arrays of tables are typed as `[]map[string]any` (so callers can iterate without per-element assertions) and homogeneous scalar arrays are `[]any`.
- `package3/python/uv/runner.go`. `Runner` interface with `Run` and `Version`. `ExecRunner` shells out to `uv` (located on PATH), enforces a 5-minute default timeout, and threads env through. `LockOptions.BuildLockArgs` renders the canonical argv tail for `uv lock`. `Lock` runs `uv lock` then reads `<projectDir>/uv.lock`; `Export` runs `uv export --format pylock.toml` and returns stdout. The bridge does not bundle uv: `Locate` searches PATH and returns a "not found" error pointing at the install docs.
- `package3/python/uv/lock.go`. Typed decoder for uv.lock. Models the schema as `Lockfile { Version, RequiresPython, Packages }` and per-package `LockedPackage { Name, Version, Source, Dependencies, Wheels, Sdist }` with `LockedSource { Kind ("registry" | "git" | "path" | "editable"), URL, Path, Reference }`. Source detection picks the first key it sees (`registry` / `git` / `path` / `editable`); the bridge does not represent the rare uv source where multiple keys coexist.
- `package3/python/uv/pylock.go`. PEP 751 round-trip. `PyLock` is the envelope, `PyLockPackage` is one resolved package, `PyLockFile` carries `Name`, `URL`, and a `Hashes` map. `Render` emits a canonical deterministic pylock.toml: packages sorted by name then version, wheels sorted by name, hash algorithms sorted alphabetically within each file. `FromLockfile` converts a uv.lock representation, deriving the file basename from the URL when uv did not record `filename`.

The deliberate "minimal TOML reader" decision is documented at the top of `package3/python/toml/doc.go`. It avoids vendoring `BurntSushi/toml` or `pelletier/go-toml/v2`. The reader is ~430 lines including the decoder; the decoder is the surface phase 8 and 9 will use to type-check upstream files defensively rather than relying on `map[string]any` lookups.

The runner runs uv with the caller's existing env plus `ExtraEnv`. The bridge will set `UV_INDEX_URL`, `UV_KEYRING_PROVIDER`, and `HTTPS_PROXY` here in phase 8 once the build orchestration knows the workspace's configured index.

## Files changed

| File | Purpose |
|------|---------|
| `package3/python/toml/doc.go` | Package doc (supported subset, rejected features) |
| `package3/python/toml/parser.go` | TOML lexer + parser (~370 lines) |
| `package3/python/toml/decoder.go` | Typed accessors over the parsed tree |
| `package3/python/toml/parser_test.go` | Parser tests (scalars, tables, arrays-of-tables, inline tables, escapes, errors) |
| `package3/python/toml/decoder_test.go` | Decoder tests (typed accessor happy + sad paths) |
| `package3/python/uv/doc.go` | Package doc (runner / lockfile / pylock split) |
| `package3/python/uv/runner.go` | `Runner` / `ExecRunner` / `Locate` / `LockOptions` / `Lock` / `Export` |
| `package3/python/uv/lock.go` | `Lockfile` / `LockedPackage` / `LockedSource` / `LockedDep` / `LockedFile` / `ParseLockfile` / `PackagesByName` / `SortedPackageNames` / `SplitHash` |
| `package3/python/uv/pylock.go` | `PyLock` / `PyLockPackage` / `PyLockFile` / `ParsePyLock` / `Render` / `FromLockfile` |
| `package3/python/uv/lock_test.go` | uv.lock fixture-based tests |
| `package3/python/uv/pylock_test.go` | PEP 751 round-trip + canonical render tests |
| `package3/python/uv/runner_test.go` | Runner abstraction + LockOptions table |
| `package3/python/uv/phase02_test.go` | `TestPhase2UvResolver` sentinel with 5 subtests |

## Test set

- `TestPhase2UvResolver/parse_uv_lock`
- `TestPhase2UvResolver/uv_lock_to_pylock`
- `TestPhase2UvResolver/pylock_round_trip`
- `TestPhase2UvResolver/runner_options`
- `TestPhase2UvResolver/md5_split_hash_rejected`
- All `package3/python/toml/...` and `package3/python/uv/...` unit tests.

## Closeout notes

The hand-rolled TOML reader pays for itself across the rest of the bridge: phase 8 will round-trip pyproject.toml between Mochi-supplied edits and the upstream tools, and phase 9 will read mochi.lock entries that the polyglot package system writes (MEP-57). Adding `BurntSushi/toml` instead would have been one line of go.mod change but it would also force every other package3 sub-package to depend on it; the in-tree mini-package keeps the dependency surface flat. The decoder layer is the explicit affordance for callers to type-check upstream files without per-element assertions, so the cost of "no library" is bounded.

The pylock.toml writer is the first canonical emitter in the bridge. Determinism is asserted across 6 iterations; downstream phases will diff against this output when comparing to `gen_releases` expectations.

No CPython runtime, no uv binary, and no network access is required for any test in this phase. The runner is exercised via a fake `Runner` so phase 9's downstream tests can pre-populate uv.lock bytes without spawning a process.
