package ts

import (
	"encoding/json"
	sitter "github.com/tree-sitter/go-tree-sitter"
	tstypescript "github.com/tree-sitter/tree-sitter-typescript/bindings/go"
)

// Inspect parses the given TypeScript source code using tree-sitter and
// returns its Program structure.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithPositions parses the source and keeps position information.
func InspectWithPositions(src string) (*Program, error) {
	return InspectWithOption(src, Option{Positions: true})
}

// InspectWithOption parses the source with the provided options.
func InspectWithOption(src string, opt Option) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(tstypescript.LanguageTypescript()))
	tree := p.Parse([]byte(src), nil)
	root := convert(tree.RootNode(), []byte(src), opt)
	if root == nil {
		root = &Node{}
	}
	return &Program{Root: (*ProgramNode)(root), Source: src}, nil
}

// MarshalJSON implements json.Marshaler for Program to provide stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
