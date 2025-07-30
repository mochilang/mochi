package elixir

import (
	sitter "github.com/smacker/go-tree-sitter"
)

// Node mirrors a tree-sitter node with minimal fields needed for JSON output.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Program represents the root syntax tree for an Elixir file.
type Program struct {
	Root Node `json:"root"`
}

// convert transforms a tree-sitter Node into our Node structure.
func convert(n *sitter.Node, src []byte) Node {
	node := Node{Kind: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
       if n.NamedChildCount() == 0 {
               node.Text = n.Content(src)
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
