package rs

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a node in the Rust syntax tree.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Rust source file composed of AST nodes.
type Program struct {
	Root *Node `json:"root"`
}

// convert turns a tree-sitter node into our AST representation.
func convert(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	out := &Node{Kind: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		out.Children = append(out.Children, convert(child))
	}
	return out
}
