package ruby

import (
	"os"

	sitter "github.com/smacker/go-tree-sitter"
	tsruby "github.com/smacker/go-tree-sitter/ruby"
)

// Node represents a node in the Ruby syntax tree as produced by tree-sitter.
type Node struct {
        Type     string `json:"type"`
        Text     string `json:"text,omitempty"`
        Line     int    `json:"line"`
        Col      int    `json:"col"`
        Children []Node `json:"children,omitempty"`
        EndLine  int    `json:"endLine"`
        EndCol   int    `json:"endCol"`
        Source   string `json:"source,omitempty"`
}

// Parse converts Ruby source code into a Node tree using tree-sitter.
func Parse(src string) (*Node, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tsruby.GetLanguage())
	tree := parser.Parse(nil, []byte(src))
	root := convert(tree.RootNode(), []byte(src))
	root.Source = src
	return &root, nil
}

// ParseFile reads Ruby source code from a file and parses it using Parse.
func ParseFile(path string) (*Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}

// convert converts a tree-sitter node into our Node type.
func convert(n *sitter.Node, src []byte) Node {
	start := n.StartPoint()
	end := n.EndPoint()
	node := Node{
		Type:    n.Type(),
		Line:    int(start.Row) + 1,
		Col:     int(start.Column),
		EndLine: int(end.Row) + 1,
		EndCol:  int(end.Column),
	}
       if n.ChildCount() == 0 {
               node.Text = n.Content(src)
       }
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, convert(child, src))
	}
	return node
}
