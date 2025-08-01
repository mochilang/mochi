package php

import "encoding/json"

// Program represents a parsed PHP source file.
// The root node is a ProgramNode mirroring the "program" rule in the grammar.
type Program struct {
	Root   *ProgramNode `json:"root,omitempty"`
	Source string       `json:"-"`
}

// Inspect parses PHP source code using tree-sitter and returns its AST.
// If opts is nil, default options are used.
func Inspect(src string, opts *Options) (*Program, error) {
	parser := newParser()
	tree := parser.Parse([]byte(src), nil)
	var o Options
	if opts != nil {
		o = *opts
	}
	root := convert(tree.RootNode(), []byte(src), o)
	if root == nil {
		return &Program{Root: nil, Source: src}, nil
	}
	return &Program{Root: (*ProgramNode)(root), Source: src}, nil
}

// InspectWithPositions behaves like Inspect but always includes location
// information in the resulting AST.
func InspectWithPositions(src string) (*Program, error) {
	opt := Options{Positions: true}
	return Inspect(src, &opt)
}

// MarshalJSON ensures stable output for Program.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
