package rb

import (
	"os"

	sitter "github.com/tree-sitter/go-tree-sitter"
	ruby "github.com/tree-sitter/tree-sitter-ruby/bindings/go"
)

// IncludePositions controls whether parsed nodes include position
// information. When false (the default) the position fields will
// be omitted from the marshalled JSON because they remain zero.
// IncludePositions controls whether parsed nodes include position
// information. When false (the default) position fields remain zero
// and are omitted from the marshalled JSON due to the `omitempty`
// struct tags.
var IncludePositions bool

// Option configures how the AST is produced when parsing source code.
// When Positions is true the parser populates Start/End information
// on all nodes.
type Option struct {
	Positions bool
}

// Node represents a simplified Ruby AST node. Only meaningful tokens
// contain text. Position information can be optionally included.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	End      int     `json:"end,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Parse converts Ruby source code into a Node tree using tree-sitter.
func Parse(src string) (*Node, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(ruby.Language()))
	tree := parser.Parse([]byte(src), nil)
	root := convertNode(tree.RootNode(), []byte(src))
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

func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}

	start := n.StartPosition()
	end := n.EndPosition()
	node := &Node{Kind: n.Kind()}

	if IncludePositions {
		node.Start = int(start.Row) + 1
		node.StartCol = int(start.Column)
		node.End = int(end.Row) + 1
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			node.Text = n.Utf8Text(src)
		} else {
			return nil
		}
	}

	for i := 0; i < int(n.NamedChildCount()); i++ {
		child := n.NamedChild(uint(i))
		if child == nil {
			continue
		}
		if c := convertNode(child, src); c != nil {
			node.Children = append(node.Children, c)
		}
	}

	if len(node.Children) == 0 && node.Text == "" {
		return nil
	}
	return node
}

func isValueNode(kind string) bool {
	switch kind {
	case "identifier", "constant", "integer", "float", "simple_symbol",
		"hash_key_symbol", "symbol", "string_content", "true", "false",
		"nil", "comment":
		return true
	default:
		return false
	}
}
