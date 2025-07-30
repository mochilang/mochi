package scheme

import (
	sitter "github.com/smacker/go-tree-sitter"
	racket "github.com/tree-sitter/tree-sitter-racket/bindings/go"
)

// Node mirrors a tree-sitter node in a simplified form.
type Node struct {
	Type     string  `json:"type"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Scheme source file.
type Program struct {
	Root *Node `json:"root"`
}

// convert recursively converts a tree-sitter node into our Node structure.
func convert(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Type: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, convert(child))
	}
	return node
}

// parse parses Scheme source using the tree-sitter Racket grammar.
func parse(src string) *Node {
	p := sitter.NewParser()
	p.SetLanguage(sitter.NewLanguage(racket.Language()))
	tree := p.Parse(nil, []byte(src))
	return convert(tree.RootNode())
}
