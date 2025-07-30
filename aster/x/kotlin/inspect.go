//go:build slow

package kotlin

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	ts "github.com/tree-sitter/go-tree-sitter/kotlin"
)

// Program represents a parsed Kotlin source file.
type Program struct {
	Root SourceFile `json:"root"`
}

// Option configures how Inspect behaves.
type Option struct {
	WithPositions bool
}

// Inspect parses Kotlin source code using tree-sitter and returns its AST.
// By default positional information is omitted from the returned tree. Pass an
// Option with WithPositions set to true to include position fields.
func Inspect(src string, opts ...Option) (*Program, error) {
	var withPos bool
	if len(opts) > 0 {
		withPos = opts[0].WithPositions
	}
	p := sitter.NewParser()
	p.SetLanguage(ts.GetLanguage())
	data := []byte(src)
	tree := p.Parse(nil, data)
	root := convert(tree.RootNode(), data, withPos)
	if root == nil {
		return &Program{}, nil
	}
	return &Program{Root: SourceFile{Node: *root}}, nil
}

// MarshalJSON ensures stable output for Program.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
