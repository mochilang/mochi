package gox

import (
	"go/ast"
	goparser "go/parser"
	"go/token"
	"strings"

	mochias "mochi/ast"
	mochiparser "mochi/parser"
)

// Node wraps a parsed Go file.
type Node struct {
	File  *ast.File
	Fset  *token.FileSet
	Lines []string
}

// Parse parses Go source code into a Node.
func Parse(src string) (*Node, error) {
	fset := token.NewFileSet()
	file, err := goparser.ParseFile(fset, "", src, goparser.ParseComments)
	if err != nil {
		return nil, err
	}
	return &Node{File: file, Fset: fset, Lines: strings.Split(src, "\n")}, nil
}

// Convert converts a parsed Go Node into a Mochi AST node.
// Only a tiny subset of Go is supported; for now this is unimplemented and
// returns an empty program.
func Convert(n *Node) (*mochias.Node, error) {
	prog, err := mochiparser.ParseString("")
	if err != nil {
		return nil, err
	}
	return mochias.FromProgram(prog), nil
}
