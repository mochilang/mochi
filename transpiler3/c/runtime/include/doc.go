// Package include is the public header tree for libmochi.
//
// Design contract: MEP-45 §3 "Runtime (libmochi)". See
// website/docs/mep/mep-0045.md.
//
// Headers live under the mochi/ subdirectory so emitted C
// uses #include "mochi/<name>.h" everywhere. Header naming
// matches the runtime surface: mochi/print.h, mochi/str.h,
// mochi/list.h, mochi/map.h, mochi/match.h, mochi/error.h,
// mochi/sched.h, ... (each one introduced by the phase that
// requires it).
//
// This directory holds only C headers; the doc.go here exists
// so that go vet ./transpiler3/c/... walks it without erroring.
package include
