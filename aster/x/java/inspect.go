//go:build slow

package java

import (
	"context"

	sitter "github.com/tree-sitter/go-tree-sitter"
	javats "github.com/tree-sitter/tree-sitter-java/bindings/go"
)

// Program represents a parsed Java source file.
// Program represents a parsed Java source file. The root node is a SourceFile.
type Program struct {
	File *SourceFile `json:"file"`
}

// Inspect parses the given Java source code using tree-sitter and returns
// its Program structure.
// Inspect parses the given Java source code using tree-sitter. When withPos is
// true the returned AST includes positional information.
func Inspect(src string, withPos bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(javats.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	root := toNode(tree.RootNode(), []byte(src), withPos)
	return &Program{File: (*SourceFile)(root)}, nil
}
