package rb

import (
	"os"

	sitter "github.com/smacker/go-tree-sitter"
	ruby "github.com/smacker/go-tree-sitter/ruby"
)

// Node represents a node in the Ruby syntax tree.
type Node struct {
	Type     string
	Value    string
	Line     int
	Col      int
	Children []Node
	EndLine  int
	EndCol   int
	Source   string
}

// Parse converts Ruby source code into a Node tree using tree-sitter.
func Parse(src string) (*Node, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(ruby.GetLanguage())
	tree := parser.Parse(nil, []byte(src))
	root := convertNode(tree.RootNode(), []byte(src))
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

func convertNode(n *sitter.Node, src []byte) Node {
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
		node.Value = n.Content(src)
	}
	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		node.Children = append(node.Children, convertNode(child, src))
	}
	return node
}
