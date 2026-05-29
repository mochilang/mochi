// Package uv bridges the Mochi build to the upstream `uv` (Astral) Python
// package resolver. The bridge invokes `uv lock` to resolve a dependency
// closure into a `uv.lock` and `uv export --format pylock.toml` to materialise
// PEP 751 cross-tool lockfiles for downstream consumers.
//
// This is the phase 2 deliverable of MEP-71. It lands three sub-packages:
//
//   - Runner: subprocess wrapper that locates the uv binary on PATH, runs
//     commands with a stable environment (HTTPS proxy honoured, PIP_INDEX_URL
//     pinned to the bridge's configured index), captures stdout / stderr, and
//     enforces a timeout.
//
//   - Lockfile: typed decoder for `uv.lock`. The fields exposed are the ones
//     the bridge needs for phase 8 (build orchestration) and phase 9 (lockfile
//     integration with mochi.lock): version, requires-python, per-package name
//     + version + source + wheel list + sdist + dependency edges.
//
//   - PyLock: PEP 751 `pylock.toml` round-trip. Read accepts the PEP 751
//     schema produced by uv, pip, poetry, and pdm. Write emits a canonical
//     PEP 751 file the bridge can hand to other tooling (downstream CI,
//     `mochi pkg publish --to=pypi` companions in phase 11).
//
// The uv binary itself is treated as an external tool and is not bundled.
// The bridge prefers a uv >= 0.5 on PATH; older versions are rejected with a
// clear error pointing at the install instructions.
package uv
