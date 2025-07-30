package java

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a minimal JSON-serialisable AST node.  Only named
// tree-sitter nodes are kept to reduce noise.  Leaf nodes store the raw
// source text in Text.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convert creates a Node from a tree-sitter node.
func convert(n *sitter.Node, src []byte) Node {
	node := Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}

	if n.NamedChildCount() == 0 {
		node.Text = n.Content(src)
		return node
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		node.Children = append(node.Children, convert(child, src))
	}
	return node
}
