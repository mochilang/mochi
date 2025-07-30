package clojure

// #cgo CFLAGS: -std=c11 -fPIC -I../../src
// #include "../../src/parser.c"
import "C"
import (
	sitter "github.com/smacker/go-tree-sitter"
	"unsafe"
)

// GetLanguage returns the tree-sitter Language for Clojure.
func GetLanguage() *sitter.Language {
	return sitter.NewLanguage(unsafe.Pointer(C.tree_sitter_clojure()))
}
