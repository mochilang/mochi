// Package src holds the libmochi implementation in C.
//
// Design contract: MEP-45 §3 "Runtime (libmochi)". See
// website/docs/mep/mep-0045.md.
//
// One *.c per logical module, named to match the corresponding
// header: src/print.c implements mochi/print.h, src/str.c
// implements mochi/str.h, and so on. Files are introduced by
// the phase whose gate requires them.
//
// This directory holds only C sources; the doc.go here exists
// so that go vet ./transpiler3/c/... walks it without erroring.
package src
