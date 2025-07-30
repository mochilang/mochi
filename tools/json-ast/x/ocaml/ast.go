package ocaml

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node in a compact JSON friendly form.
// Leaf nodes store their source text in the Text field. Only named
// children are preserved to keep the structure small.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
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
