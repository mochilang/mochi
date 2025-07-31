//go:build slow

package rkt

import (
	"encoding/json"

	sitter "github.com/tree-sitter/go-tree-sitter"
	racket "github.com/tree-sitter/tree-sitter-racket/bindings/go"
)

// Program represents a parsed Racket source file.
type Program struct {
	Root *ProgramNode `json:"root"`
	Lang string       `json:"lang,omitempty"`
}

// MarshalJSON implements json.Marshaler for Program to ensure stable output order.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}

// Options control how Inspect produces the AST.
type Options struct {
	// Positions includes line/column information when true.
	Positions bool
}

// Inspect parses Racket source code using tree-sitter.
// By default positional information is omitted from the returned AST. Pass
// Options{Positions: true} to include it.
func Inspect(src string, opts ...Options) (*Program, error) {
	var opt Options
	if len(opts) > 0 {
		opt = opts[0]
	}
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(racket.Language()))
	data := []byte(src)
	tree := p.Parse(data, nil)
	prog := convertProgram(tree.RootNode(), data, opt.Positions)
	if prog == nil {
		prog = &Program{}
	}
	if prog.Root != nil {
		for _, c := range prog.Root.Children {
			if c.Kind == "extension" && len(c.Children) > 0 {
				prog.Lang = c.Children[0].Text
				break
			}
		}
	}
	return prog, nil
}
