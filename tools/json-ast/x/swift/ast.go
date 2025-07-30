package swift

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a node in the Swift syntax tree.
// Each node carries its kind and byte offsets as reported by tree-sitter.
// Children only include named nodes to keep the structure compact.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// SourceFile is the root of a Swift AST.
type SourceFile struct {
	Node
}

// convertNode transforms a tree-sitter node into the Go AST representation.
// The conversion is recursive and ignores anonymous children.
func convertNode(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	out := &Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		out.Children = append(out.Children, convertNode(child))
	}
	return out
}

// ConvertFile converts the tree-sitter root node of a Swift file into a
// SourceFile AST value.
func ConvertFile(n *sitter.Node) *SourceFile {
	if n == nil {
		return nil
	}
	return &SourceFile{Node: *convertNode(n)}
}
