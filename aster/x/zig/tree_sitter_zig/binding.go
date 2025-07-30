package tree_sitter_zig

/*
#include "parser.h"
extern const TSLanguage *tree_sitter_zig(void);
*/
import "C"
import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	"unsafe"
)

func GetLanguage() *sitter.Language {
	ptr := unsafe.Pointer(C.tree_sitter_zig())
	return sitter.NewLanguage(ptr)
}
