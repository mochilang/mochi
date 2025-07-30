package elixir

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	tslang "github.com/tree-sitter/tree-sitter-elixir/bindings/go"
)

// Inspect parses Elixir source code using tree-sitter and returns the Program.
// Inspect parses Elixir source code using tree-sitter and returns the Program.
// When includePos is false the resulting JSON omits all position information.
func Inspect(src string, includePos bool) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tslang.Language()))
	tree := parser.Parse([]byte(src), nil)
	n := convert(tree.RootNode(), []byte(src), includePos)
	if n == nil {
		return &Program{Root: nil}, nil
	}
	root := &Source{Node: *n}
	return &Program{Root: root}, nil
}
