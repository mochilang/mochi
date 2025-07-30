package dart

import (
	"encoding/json"

	ts "github.com/UserNobody14/tree-sitter-dart/bindings/go"
	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a node in the Dart syntax tree.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Dart source file.
type Program struct {
	File *Node `json:"file"`
}

// Inspect parses Dart source code using tree-sitter and returns
// its Program structure.
func Inspect(src string) (*Program, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(ts.Language()))
	tree := parser.Parse(nil, []byte(src))
	return &Program{File: toNode(tree.RootNode())}, nil
}

func toNode(n *sitter.Node) *Node {
	if n == nil {
		return nil
	}
	node := &Node{Kind: n.Type(), Start: int(n.StartByte()), End: int(n.EndByte())}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		node.Children = append(node.Children, toNode(n.NamedChild(i)))
	}
	return node
}

// MarshalJSON ensures stable output ordering.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
