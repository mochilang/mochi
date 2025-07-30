package php

import "encoding/json"

// Program holds the statements of a PHP source file after conversion to a
// minimal AST form.
type Program struct {
	Statements []*Node `json:"statements,omitempty"`
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
	var stmts []*Node
	if root != nil {
		stmts = root.Children
	}
	return &Program{Statements: stmts}, nil
}

// MarshalJSON ensures stable output for Program.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
