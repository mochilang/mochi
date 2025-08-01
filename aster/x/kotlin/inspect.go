//go:build slow

package kotlin

import (
	"encoding/json"

	ts "github.com/tree-sitter-grammars/tree-sitter-kotlin/bindings/go"
	sitter "github.com/tree-sitter/go-tree-sitter"
)

// Program represents a parsed Kotlin source file.
type Program struct {
	Root   SourceFile `json:"root"`
	Source string     `json:"-"`
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
	p.SetLanguage(sitter.NewLanguage(ts.Language()))
	data := []byte(src)
	tree := p.Parse(data, nil)
	root := convert(tree.RootNode(), data, withPos)
	if root == nil {
		return &Program{Source: src}, nil
	}
	return &Program{Root: SourceFile{Node: *root}, Source: src}, nil
}

// MarshalJSON ensures stable output for Program.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
