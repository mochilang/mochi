package haskell

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a tree-sitter node.
// Leaf nodes record their source text in the Text field while
// inner nodes only keep their kind and byte offsets.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// convert transforms a tree-sitter node into the Node structure defined above.
// Only named children are traversed to keep the result compact.
func convert(n *sitter.Node, src []byte) Node {
	out := Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
	if n.NamedChildCount() == 0 {
		out.Text = n.Content(src)
		return out
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if child == nil {
			continue
		}
		out.Children = append(out.Children, convert(child, src))
	}
	return out
}
