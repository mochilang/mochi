// Package c hosts the C-target runtime source files compiler3/emit/c
// generates calls into. The package's Go side is only a manifest:
// it lists the .h and .c files via embed.FS so the build driver can
// drop them next to the emitted source before invoking cc.
//
// Why a Go package wrapping pure C files: it preserves the MEP-42
// no-cgo identity. The Mochi binary never links against the runtime;
// instead the user's compiled binary (produced by `mochi build
// --target=c`) does, after the driver writes the runtime files to
// the build directory. Embedding means `go install mochi` ships
// the runtime inside the mochi binary, satisfying the "single tool
// to bootstrap" property the AOT-track wants.
//
// See compiler3/build/c for the driver-side use.
package c

import "embed"

// Runtime carries every C-source file compiler3/build/c writes to
// the build directory next to the emitted gen.c. The driver iterates
// the directory entries and writes each one verbatim. The files
// live under src/ rather than the package root because Go's build
// rules refuse to load a package directory that contains loose .c
// files when cgo is off (and the whole point of this package is to
// stay no-cgo on the build host).
//
//go:embed src/print.h src/print.c src/mochi_list_i64.h src/mochi_list_i64.c src/mochi_f64_array.h src/mochi_f64_array.c src/mochi_str_array.h src/mochi_str_array.c src/mochi_time.h src/mochi_time.c src/mochi_map_i64_i64.h src/mochi_map_i64_i64.c src/mochi_tree.h src/mochi_tree.c src/mochi_str.h src/mochi_str.c
var Runtime embed.FS
