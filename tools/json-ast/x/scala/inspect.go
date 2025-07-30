package scala

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	tscala "github.com/smacker/go-tree-sitter/scala"
)

// Inspect parses Scala source code using tree-sitter and returns its AST.
func Inspect(src string, opts ...bool) (*Program, error) {
	includePos := false
	if len(opts) > 0 {
		includePos = opts[0]
	}
	parser := sitter.NewParser()
	parser.SetLanguage(tscala.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	n := convert(tree.RootNode(), []byte(src), includePos)
	if n == nil {
		return &Program{}, nil
	}
	cu := &CompilationUnit{*n}
	return &Program{Root: cu}, nil
}
