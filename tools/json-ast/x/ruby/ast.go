package ruby

import (
	"os"

	sitter "github.com/smacker/go-tree-sitter"
	tsruby "github.com/smacker/go-tree-sitter/ruby"
)

// Node represents a node in the Ruby syntax tree as produced by tree-sitter.
// Node represents a tree-sitter node in a simplified form. Only leaf nodes
// store their raw source text via the Text field. Position information is
// expressed in byte offsets to keep the structure minimal.
type Node struct {
	Kind     string `json:"kind"`
	Start    int    `json:"start"`
	End      int    `json:"end"`
	Text     string `json:"text,omitempty"`
	Children []Node `json:"children,omitempty"`
}

// Parse converts Ruby source code into a Node tree using tree-sitter.
// Parse converts Ruby source code into a Node tree using tree-sitter.
func Parse(src string) (*Node, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(tsruby.GetLanguage())
	data := []byte(src)
	tree := parser.Parse(nil, data)
	root := convert(tree.RootNode(), data)
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
	node := Node{
		Kind:  n.Type(),
		Start: int(n.StartByte()),
		End:   int(n.EndByte()),
	}
	if n.ChildCount() == 0 {
		node.Text = n.Content(src)
		return node
	}
	for i := 0; i < int(n.ChildCount()); i++ {
		child := n.Child(i)
		if child == nil {
			continue
		}
		node.Children = append(node.Children, convert(child, src))
	}
	return node
}
