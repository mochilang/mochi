package kotlin

import (
	"encoding/json"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/kotlin"
)

// Program represents a parsed Kotlin source file.
type Program struct {
	Root SourceFile `json:"root"`
}

// Inspect parses Kotlin source code using tree-sitter and returns its AST.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(ts.GetLanguage())
	data := []byte(src)
	tree := p.Parse(nil, data)
	root := convert(tree.RootNode(), data)
	return &Program{Root: SourceFile{Node: root}}, nil
}

// MarshalJSON ensures stable output for Program.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
