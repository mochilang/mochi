//go:build slow

package zig

import (
	"context"
	"fmt"

	tszig "github.com/slimsag/tree-sitter-zig/bindings/go"
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a tree-sitter node in Zig's syntax tree.
type Node struct {
	Type     string `json:"type"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Program describes a parsed Zig source file.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses Zig source code using tree-sitter.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tszig.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	root := convertNode(tree.RootNode(), []byte(src))
	return &Program{Root: root}, nil
}

func convertNode(n *sitter.Node, src []byte) Node {
	node := Node{
		Type:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
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
