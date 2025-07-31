package ruby

import (
	"os"

	sitter "github.com/tree-sitter/go-tree-sitter"
	tsruby "github.com/tree-sitter/tree-sitter-ruby/bindings/go"
)

// Options controls how the AST is produced when parsing source code.
// When IncludePositions is false (the default) all positional fields
// remain zero and are omitted from the JSON due to the `omitempty`
// struct tags.
type Options struct {
	IncludePositions bool
}

// Node represents a node in the Ruby syntax tree as produced by tree-sitter.
// Node represents a tree-sitter node in a simplified form. Only leaf nodes
// store their raw source text via the Text field. Position information is
// expressed in byte offsets to keep the structure minimal.
// Node mirrors a tree-sitter node in a compact form suitable for JSON
// serialisation. Position information is stored using 1-based line numbers
// and 0-based column offsets. Leaf nodes that carry a value expose it via the
// Text field. Pure syntax tokens are omitted entirely.
type Node struct {
	Kind     string  `json:"kind"`
	Text     string  `json:"text,omitempty"`
	Start    int     `json:"start,omitempty"`
	End      int     `json:"end,omitempty"`
	StartCol int     `json:"startCol,omitempty"`
	EndCol   int     `json:"endCol,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// A small set of concrete node types used in the golden example. They simply
// embed Node so the JSON representation keeps the same shape while giving the
// Go compiler a richer type hierarchy.
type (
	ProgramNode             struct{ Node }
	Comment                 struct{ Node }
	Assignment              struct{ Node }
	Call                    struct{ Node }
	ArgumentList            struct{ Node }
	Array                   struct{ Node }
	Pair                    struct{ Node }
	Identifier              struct{ Node }
	Constant                struct{ Node }
	Integer                 struct{ Node }
	String                  struct{ Node }
	StringContent           struct{ Node }
	SimpleSymbol            struct{ Node }
	HashKeySymbol           struct{ Node }
	True                    struct{ Node }
	Binary                  struct{ Node }
	Begin                   struct{ Node }
	DoBlock                 struct{ Node }
	BlockParameters         struct{ Node }
	BodyStatement           struct{ Node }
	ElementReference        struct{ Node }
	ParenthesizedStatements struct{ Node }
)

// Parse converts Ruby source code into a Node tree using tree-sitter.
// Parse converts Ruby source code into a Node tree using tree-sitter.
func Parse(src string, opts ...Options) (*Node, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(sitter.NewLanguage(tsruby.Language()))
	data := []byte(src)
	tree := parser.Parse(data, nil)
	var withPos bool
	if len(opts) > 0 {
		withPos = opts[0].IncludePositions
	}
	root := convert(tree.RootNode(), data, withPos)
	if root == nil {
		return nil, nil
	}
	return root, nil
}

// ParseFile reads Ruby source code from a file and parses it using Parse.
func ParseFile(path string, opts ...Options) (*Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data), opts...)
}

// convert converts a tree-sitter node into our Node representation. Pure
// syntax leaves are dropped so the resulting JSON only contains nodes that
// carry actual values or have children.
func convert(n *sitter.Node, src []byte, pos bool) *Node {
	if n == nil {
		return nil
	}

	node := &Node{Kind: n.Kind()}
	if pos {
		start := n.StartPosition()
		end := n.EndPosition()
		node.Start = int(start.Row) + 1
		node.End = int(end.Row) + 1
		node.StartCol = int(start.Column)
		node.EndCol = int(end.Column)
	}

	if n.NamedChildCount() == 0 {
		if isValueNode(n.Kind()) {
			node.Text = string(src[n.StartByte():n.EndByte()])
		} else if keepEmptyNode(n.Kind()) {
			node.Text = string(src[n.StartByte():n.EndByte()])
		} else {
			return nil
		}
	}

	if n.ChildCount() > n.NamedChildCount() {
		switch node.Kind {
		case "binary", "range":
			if n.ChildCount() >= 3 {
				op := n.Child(1)
				node.Text = string(src[op.StartByte():op.EndByte()])
			}
		case "unary":
			if n.ChildCount() >= 2 {
				op := n.Child(0)
				node.Text = string(src[op.StartByte():op.EndByte()])
			}
		}
	}

	for i := uint(0); i < n.NamedChildCount(); i++ {
		child := n.NamedChild(i)
		if c := convert(child, src, pos); c != nil {
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
	case "identifier", "constant", "integer", "string", "string_content",
		"hash_key_symbol", "simple_symbol", "true", "false", "comment",
		"next", "break", "global_variable":
		return true
	default:
		return false
	}
}

// keepEmptyNode reports whether nodes with the given kind should be retained
// even when they have no children and no textual content. This is required for
// constructs like empty arrays so the printer can faithfully reconstruct the
// source code.
func keepEmptyNode(kind string) bool {
	switch kind {
	case "array", "argument_list", "method_parameters", "block_parameters",
		"next", "break":
		return true
	default:
		return false
	}
}
