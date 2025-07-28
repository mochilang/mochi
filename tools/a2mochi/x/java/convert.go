package java

import (
	mochias "mochi/ast"
	mochiparser "mochi/parser"
	"strings"
)

// Node holds parsed Java source. Parsing is not implemented yet.
type Node struct {
	Lines []string
}

// Parse parses Java source code into a Node. Only splitting lines is done for now.
func Parse(src string) (*Node, error) {
	return &Node{Lines: strings.Split(src, "\n")}, nil
}

// Convert converts a parsed Java Node into a Mochi AST node.
// The converter is currently unimplemented and returns an empty program.
func Convert(n *Node) (*mochias.Node, error) {
	prog, err := mochiparser.ParseString("")
	if err != nil {
		return nil, err
	}
	return mochias.FromProgram(prog), nil
}
