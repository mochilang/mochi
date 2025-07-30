package erlang

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a simplified Erlang AST node converted from tree-sitter.
//
// Leaf nodes contain the raw source text in the Text field while internal
// nodes only record their kind and children.  Start and End are byte offsets
// within the source.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Text     string  `json:"text,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// convert converts a tree-sitter Node into our Node representation.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	node := &Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}

	// Only keep raw text for leaf nodes.  Using NamedChildCount filters out
	// punctuation and other non-semantic tokens so the resulting JSON is
	// much smaller while still containing the important information.
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
