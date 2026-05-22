// Package zig vendors the zig cross-compiler that the
// transpiler3/c build driver falls back to when a host C
// compiler is not discoverable, and uses by default for cross
// targets (every triple in MEP-45 §9 tier-1 matrix).
//
// Design contract: MEP-45 §11 "Build driver", Phase 1.3, and
// Phase 11 "Cross-compile tier-1 matrix". See
// website/docs/mep/mep-0045.md.
//
// Public entry point (introduced incrementally by phase):
//
//	func EnsureInstalled(version string) (cc, ar string, err error)
//
// Behaviour:
//
//   - Pinned by SHA-256 manifest in this package. The current
//     pinned release is zig 0.16.0; matching upstream tarball
//     URLs and SHA-256s live in transpiler3/c/toolchain/zig/
//     manifest.go.
//   - Cached under $XDG_CACHE_HOME/mochi/zig/<version>/.
//   - First call on a fresh machine downloads the
//     host-architecture tarball, verifies the SHA-256, extracts
//     to the cache directory, and returns (cc, ar) paths.
//   - Subsequent calls return cached paths without touching the
//     network (stat-only).
//   - Returns wrappers that invoke "zig cc" and "zig ar" with
//     the right -target flag pre-baked for a requested triple
//     (callers see drop-in cc/ar binaries).
//
// Phase 0 ships the package skeleton; Phase 1.3 wires the
// download + SHA-256 verification path; Phase 11 adds
// per-tier-1-triple test coverage.
package zig
