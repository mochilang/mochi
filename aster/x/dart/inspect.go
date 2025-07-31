package dart

import (
	"encoding/json"

	ts "github.com/UserNobody14/tree-sitter-dart/bindings/go"
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Program represents a parsed Dart source file.
type Program struct {
	Root *ProgramNode `json:"root"`
}

// Inspect parses Dart source code using tree-sitter and returns
// its Program structure.
func Inspect(src string) (*Program, error) {
	return InspectWithOptions(src, Options{})
}

// InspectWithOptions parses Dart source code using tree-sitter and returns its
// Program structure configured by opts.
func InspectWithOptions(src string, opts Options) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(ts.Language()))
	tree := parser.Parse([]byte(src), nil)
	root := (*ProgramNode)(toNode(tree.RootNode(), []byte(src), opts.IncludePos))
	if root == nil {
		root = &ProgramNode{}
	}
	return &Program{Root: root}, nil
}

// MarshalJSON ensures stable output ordering.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
