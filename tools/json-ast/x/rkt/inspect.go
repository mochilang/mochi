package rkt

import (
	sitter "github.com/smacker/go-tree-sitter"
	racket "github.com/tree-sitter/tree-sitter-racket/bindings/go"
)

// Program represents a parsed Racket source file.
type Program struct {
	Forms []*Node `json:"forms"`
}

// Inspect parses Racket source code using tree-sitter and returns a Program
// describing its syntax tree.
func Inspect(src string) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(racket.Language()))
	tree := p.Parse(nil, []byte(src))
	return convertProgram(tree.RootNode(), []byte(src)), nil
}
