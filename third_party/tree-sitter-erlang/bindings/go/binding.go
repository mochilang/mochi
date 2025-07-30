package erlang

// #cgo CFLAGS: -std=c11 -fPIC -I../../src
// #include "../../src/parser.c"
import "C"

import (
	"unsafe"

	sitter "github.com/tree-sitter/go-tree-sitter"
)

// GetLanguage returns the tree-sitter Language for Erlang.
func GetLanguage() *sitter.Language {
	ptr := unsafe.Pointer(C.tree_sitter_erlang())
	return sitter.NewLanguage(ptr)
}
