//go:build slow

package java

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	javats "github.com/smacker/go-tree-sitter/java"
)

// Node represents a tree-sitter node in a generic form.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Program represents a parsed Java source file.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses the given Java source code using tree-sitter and returns
// its Program structure.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(javats.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}

func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{Kind: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
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
