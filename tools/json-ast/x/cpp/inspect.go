package cpp

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	ts "github.com/smacker/go-tree-sitter/cpp"
)

// Node represents a tree-sitter node in a generic form.
type Node struct {
	Type     string `json:"type"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Program is the root of a parsed C++ translation unit.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses the given C++ source code using tree-sitter and
// returns its Program structure.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(ts.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}

// convertNode converts a tree-sitter node into our Node type.
func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{Type: n.Type()}
	if n.ChildCount() == 0 {
		node.Text = n.Content(src)
	}
	for i := 0; i < int(n.ChildCount()); i++ {
		child := n.Child(i)
		if child == nil {
			continue
		}
		node.Children = append(node.Children, convertNode(child, src))
	}
	return node
}
