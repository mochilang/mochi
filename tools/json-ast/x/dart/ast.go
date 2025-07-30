package dart

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a node in the Dart syntax tree using a typed
// structure. Leaf nodes store their raw source text in Value.
type Node struct {
	Type     string `json:"type"`
	Value    string `json:"value,omitempty"`
	Line     int    `json:"line"`
	Col      int    `json:"col"`
	Children []Node `json:"children,omitempty"`
	EndLine  int    `json:"endLine"`
	EndCol   int    `json:"endCol"`
	Source   string `json:"source,omitempty"`
}

// convertNode recursively converts the given tree-sitter node to a Node
// using the provided source code for leaf values.
func convertNode(n *sitter.Node, src []byte) Node {
	start := n.StartPoint()
	end := n.EndPoint()
	node := Node{
		Type:    n.Type(),
		Line:    int(start.Row) + 1,
		Col:     int(start.Column),
		EndLine: int(end.Row) + 1,
		EndCol:  int(end.Column),
	}
	if n.ChildCount() == 0 {
		node.Value = n.Content(src)
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, convertNode(child, src))
	}
	return node
}
