package rkt

import (
	sitter "github.com/tree-sitter/go-tree-sitter"
	racket "github.com/tree-sitter/tree-sitter-racket/bindings/go"
)

// Program represents a parsed Racket source file.
type Program struct {
	Root *ProgramNode `json:"root"`
}

// Inspect parses Racket source code using tree-sitter and returns a Program
// describing its syntax tree.
// Inspect parses Racket source code using tree-sitter.  If withPos is true the
// resulting AST nodes include line and column information.
func Inspect(src string, withPos bool) (*Program, error) {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(racket.Language()))
	tree := p.Parse([]byte(src), nil)
	return convertProgram(tree.RootNode(), []byte(src), withPos), nil
}
