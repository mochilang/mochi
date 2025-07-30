//go:build slow

package scheme

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tsscheme "github.com/tree-sitter/tree-sitter-scheme/bindings/go"
)

// Inspect parses Scheme source code using tree-sitter and returns a Program
// describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tsscheme.Language()))
	tree := p.Parse([]byte(src), nil)
	return convertProgram(tree.RootNode(), []byte(src)), nil
}
