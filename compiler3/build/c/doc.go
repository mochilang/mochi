// Package cbuild is the C-target build driver for MEP-42 Phase 4.0
// (single-file native binary via system cc). It owns the user-facing
// options `mochi build --target=c` will accept; the CLI subcommand
// in cmd/mochi/main.go invokes Build with a fully-formed Program and
// an Options carrying OutDir / KeepEmit / CC / CCFlags / Static.
//
// # Phase 4.0 scope
//
// The driver emits one .c file under OutDir, invokes the system cc
// (default: "cc"; overridable via Options.CC or the environment) to
// produce a single native executable, and returns the path to that
// executable. The cc flag set is fixed at `-std=c99 -O2`; callers
// can append flags via Options.CCFlags. The driver does not invoke
// LLD directly in Phase 4.0; the system cc's default linker is
// trusted to produce a working ELF/Mach-O on the four phase-1
// targets.
//
// # The single-binary gate
//
// MEP-42's top-line objective is `mochi build hello.mochi` producing
// a single native binary that runs on a clean machine of the target
// platform. The driver enforces this by:
//
//   - Producing exactly one output file (the binary; the .c file is
//     deleted on success unless KeepEmit is set).
//   - Refusing to invoke cc with any flag that would force a runtime
//     Mochi dependency. Phase 4.0 emits no `#include` outside libc,
//     so the produced binary's only shared-object dependency is the
//     system libc (which is present on every supported target).
//   - The Static field (Options.Static) wires `-static` for callers
//     who want musl static-PIE on Linux; Phase 5+ widens this to
//     cross-target static linking.
//
// # Identity rule
//
// The driver shells to whatever cc the user has. It does not bundle
// a C compiler, does not depend on libclang, does not link in cgo.
// The shipping mochi binary remains pure-Go-no-cgo. See MEP-42 §13.
package cbuild
