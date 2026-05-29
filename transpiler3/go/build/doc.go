// Package build is the transpiler3/go driver: source-to-binary
// in one call.
//
// Design contract: MEP-54 §Phase 1+. See
// website/docs/mep/mep-0054.md.
//
// Public entry point (introduced incrementally by phase):
//
//	type Driver struct{ ... }
//	func (d *Driver) Build(src, out string, target, profile string) error
//
// Pipeline:
//
//  1. Parse + type-check (shared with vm3).
//  2. Lower to aotir.Program (shared with MEP-45..53).
//  3. Lower aotir to gotree.File (MEP-54 lower package).
//  4. Render to .go source files (MEP-54 emit package).
//  5. Write a go.mod referencing dev.mochilang/runtime/go via
//     a `replace` directive to the vendored runtime tree.
//  6. Invoke `go build` in the work directory.
//  7. Copy the produced binary to the caller's --out path.
//
// Targets:
//
//   - go-binary-{linux,darwin,windows,freebsd}-{amd64,arm64}:
//     statically linked Go binary cross-compiled via GOOS / GOARCH.
//   - go-module: a publishable Go module zip with main package
//     omitted (library-only consumption).
//   - go-wasm-{js,wasip1}: WebAssembly module produced via the
//     stdlib js and wasip1 targets.
//
// Profiles:
//
//   - debug:   `-gcflags=all=-N -l`, no symbol stripping.
//   - release: default optimisations + `-ldflags=-s -w` for size.
//
// Phase 0 ships the package skeleton. Phase 1 wires the
// hello-world end-to-end path.
package build
