package cpp

import (
	"context"
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	ts "github.com/tree-sitter/tree-sitter-cpp/bindings/go"
)

// Program is the root of a parsed C++ translation unit.
// Program represents the root of a parsed translation unit. It mirrors the
// tree-sitter structure using the minimal Node defined in ast.go.
type Program struct {
	Root *TranslationUnit `json:"root"`
}

// Inspect parses the given C++ source code using tree-sitter and
// returns its Program structure.
// Inspect parses the given C++ source code using tree-sitter and
// returns its Program structure. Position information is omitted unless
// opts.WithPositions is set to true.
func Inspect(src string, opts ...Options) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(ts.Language()))
	tree := parser.ParseCtx(context.Background(), []byte(src), nil)
	var o Options
	if len(opts) > 0 {
		o = opts[0]
	}
	root := convert(tree.RootNode(), []byte(src), o)
	if root == nil {
		return &Program{}, nil
	}
	return &Program{Root: (*TranslationUnit)(root)}, nil
}

// MarshalJSON ensures stable output ordering.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
