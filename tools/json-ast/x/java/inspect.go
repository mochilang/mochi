package java

import (
	"context"
	"fmt"

	sitter "github.com/smacker/go-tree-sitter"
	tsjava "github.com/smacker/go-tree-sitter/java"
)

// Node represents a node in the Java syntax tree.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Java source file.
type Program struct {
	Root *Node `json:"root"`
}

// Inspect parses the given Java source code using tree-sitter and returns a Program.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tsjava.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, []byte(src))
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}
	return &Program{Root: toNode(tree.RootNode())}, nil
}

func toNode(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, toNode(child))
	}
	return node
}
