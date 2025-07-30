package ruby

import (
	"os"

	sitter "github.com/smacker/go-tree-sitter"
	tsruby "github.com/smacker/go-tree-sitter/ruby"
)

// IncludePositions controls whether parsed nodes include position
// information. When false (the default) the JSON output omits all
// positional fields to keep the representation small.
var IncludePositions bool

// Node represents a node in the Ruby syntax tree as produced by tree-sitter.
// Node represents a tree-sitter node in a simplified form. Only leaf nodes
// store their raw source text via the Text field. Position information is
// expressed in byte offsets to keep the structure minimal.
// Node mirrors a tree-sitter node in a compact form suitable for JSON
// serialisation. Position information is stored using 1-based line numbers
// and 0-based column offsets. Leaf nodes that carry a value expose it via the
// Text field. Pure syntax tokens are omitted entirely.
type Node struct {
	Kind     string `json:"kind"`
	Text     string `json:"text,omitempty"`
	Start    int    `json:"start,omitempty"`
	StartCol int    `json:"startCol,omitempty"`
	End      int    `json:"end,omitempty"`
	EndCol   int    `json:"endCol,omitempty"`
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
	if root == nil {
		return nil, nil
	}
	return root, nil
}

// ParseFile reads Ruby source code from a file and parses it using Parse.
func ParseFile(path string) (*Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}

// convert converts a tree-sitter node into our Node representation. Pure
// syntax leaves are dropped so the resulting JSON only contains nodes that
// carry actual values or have children.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	node := &Node{Kind: n.Type()}
	if IncludePositions {
		start := n.StartPoint()
		end := n.EndPoint()
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Type()) {
			node.Text = n.Content(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(i)
		if c := convert(child, src); c != nil {
			node.Children = append(node.Children, *c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "constant", "integer", "string", "string_content",
		"hash_key_symbol", "simple_symbol", "true", "false", "comment":
		return true
	default:
		return false
	}
}
