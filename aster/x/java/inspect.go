//go:build slow

package java

import (
	"context"

	sitter "github.com/tree-sitter/go-tree-sitter"
	javats "github.com/tree-sitter/tree-sitter-java/bindings/go"
)

// Program represents a parsed Java source file.
// Program represents a parsed Java source file. The root node is a SourceFile.
// Program represents a parsed Java source file. The File field holds the root
// program node of the AST.
type Program struct {
	File *ProgramNode `json:"file"`
}

// Inspect parses the given Java source code using tree-sitter and returns
// its Program structure.
// Inspect parses the given Java source code using tree-sitter. When withPos is
// true the returned AST includes positional information.
// Inspect parses the given Java source code using tree-sitter. When opt.Positions
// is true positional information is recorded in the resulting AST.
func Inspect(src string, opt Options) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(javats.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	root := convert(tree.RootNode(), []byte(src), opt.Positions)
	return &Program{File: (*ProgramNode)(root)}, nil
}
