package erlang

/*
#cgo CFLAGS: -std=c11 -I${SRCDIR} -I${SRCDIR}/tree_sitter
*/
import "C"

// This file ensures the external scanner is compiled and linked. The scanner
// implementation lives in scanner.c which is compiled together with this
// package by cgo.
