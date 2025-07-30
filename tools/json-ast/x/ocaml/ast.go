package ocaml

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node in a compact JSON friendly form.
// Leaf nodes store their source text in the Text field. Only named
// children are preserved to keep the structure small.
// Node represents a simplified OCaml AST node. Only nodes that carry
// meaningful values keep their text to minimise the resulting JSON.
// Source positions include both line and column information.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start"`
	StartCol int    `json:"startCol"`
	End      int    `json:"end"`
	EndCol   int    `json:"endCol"`
	Children []Node `json:"children,omitempty"`
}

// File is the root syntactic element of an OCaml source file.
type File struct{ Node }

// Program wraps a parsed OCaml source file.
type Program struct {
	File File `json:"file"`
}

// convert recursively converts a tree-sitter node into a Node.
// Leaf nodes keep their raw text while internal nodes only preserve
// their named children.
// convert recursively converts a tree-sitter node into our Node structure.
// Leaf nodes that don't carry useful values are skipped entirely.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	sp := n.StartPoint()
	ep := n.EndPoint()
	node := &Node{
		Kind:     n.Type(),
		Start:    int(sp.Row) + 1,
		StartCol: int(sp.Column),
		End:      int(ep.Row) + 1,
		EndCol:   int(ep.Column),
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			node.Text = n.Content(src)
			return node
		}
		return nil
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := convert(n.NamedChild(i), src)
		if child != nil {
			node.Children = append(node.Children, *child)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

// isValueNode reports whether a node kind holds a textual value that should be kept.
func isValueNode(kind string) bool {
	switch kind {
	case "comment", "constructor_name", "module_name", "number",
		"rel_operator", "string", "string_content", "unit",
		"value_name", "value_pattern":
		return true
	default:
		return false
	}
}
