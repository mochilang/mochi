package php

import "encoding/json"

// Program represents a parsed PHP file.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses PHP source code using tree-sitter and returns its AST.
// If opts is nil, default options are used.
func Inspect(src string, opts *Options) (*Program, error) {
	parser := newParser()
	tree := parser.Parse(nil, []byte(src))
	var o Options
	if opts != nil {
		o = *opts
	}
	root := convert(tree.RootNode(), []byte(src), o)
	return &Program{Root: root}, nil
}

// MarshalJSON ensures stable output for Program.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
