// Package build is the transpiler3/c driver: source-to-binary
// in one call, with content-addressed caching, host C compiler
// discovery, and vendored zig fallback.
//
// Design contract: MEP-45 §11 "Build driver". See
// website/docs/mep/mep-0045.md.
//
// Public entry point (introduced incrementally by phase):
//
//	type Driver struct{ ... }
//	func (d *Driver) Build(src, out string, target, profile string) error
//
// Steps:
//
//  1. Parse + type-check + lower + emit. Output: C source +
//     compile command.
//  2. Cache key. BLAKE3(transpiler version || profile ||
//     target triple || canonicalised source-set bytes). Cache
//     layout: .mochi/cache/{ast,aotir,c,obj,bin}/<key>.
//  3. Compiler discovery. $CC first, then cc, clang, gcc on
//     PATH; on miss, fall back to the vendored zig cc (Phase
//     1.3, transpiler3/c/toolchain/zig).
//  4. Compile + link. Always statically linked unless the user
//     explicitly opts in to a dynamic libc.
//  5. Reproducibility flags (Phase 17). SOURCE_DATE_EPOCH,
//     -ffile-prefix-map=$PWD=., -fdebug-prefix-map=$PWD=., no
//     __DATE__ / __TIME__ in generated output.
//
// Profiles:
//
//   - debug:   -O0 -g, asserts on, sanitiser hooks present.
//   - release: -O2, asserts compiled out, frame pointer kept
//              for profiler tooling.
//   - sanitise-{asan,ubsan,tsan,msan}: release flags plus
//              the named sanitiser (Phase 16).
//
// Phase 0 ships the package skeleton. Phase 1 wires the
// hello-world end-to-end path. Later phases add the cache layer,
// vendored zig fallback, and cross-compile targeting.
package build
