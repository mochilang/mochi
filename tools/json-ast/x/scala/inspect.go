package scala

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	tscala "github.com/smacker/go-tree-sitter/scala"
)

// Node represents a tree-sitter node.
type Node struct {
	Type     string `json:"type"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Program represents a parsed Scala source file.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses Scala source code using tree-sitter and returns its AST.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tscala.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}

func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{Type: n.Type()}
	if n.ChildCount() == 0 {
		node.Text = n.Content(src)
		return node
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
