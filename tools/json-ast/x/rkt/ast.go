package rkt

import sitter "github.com/smacker/go-tree-sitter"

// Node represents a simplified Racket AST node.
type Node struct {
	Symbol  string  `json:"symbol,omitempty"`
	String  string  `json:"string,omitempty"`
	Number  string  `json:"number,omitempty"`
	Lang    string  `json:"lang,omitempty"`
	Comment string  `json:"comment,omitempty"`
	List    []*Node `json:"list,omitempty"`
	Quote   *Node   `json:"quote,omitempty"`
}

// convertProgram converts the root tree-sitter node into a Program.
func convertProgram(root *sitter.Node, src []byte) *Program {
	if root == nil {
		return &Program{}
	}
	var forms []*Node
	for i := 0; i < int(root.NamedChildCount()); i++ {
		forms = append(forms, convert(root.NamedChild(i), src))
	}
	return &Program{Forms: forms}
}

// convert converts a tree-sitter node into a Node structure.
func convert(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	switch n.Type() {
	case "comment":
		return &Node{Comment: n.Content(src)}
	case "extension":
		if child := n.NamedChild(0); child != nil {
			return &Node{Lang: child.Content(src)}
		}
		return &Node{}
	case "list", "program":
		list := make([]*Node, 0, n.NamedChildCount())
		for i := 0; i < int(n.NamedChildCount()); i++ {
			list = append(list, convert(n.NamedChild(i), src))
		}
		return &Node{List: list}
	case "symbol", "lang_name":
		return &Node{Symbol: n.Content(src)}
	case "string":
		return &Node{String: n.Content(src)}
	case "number":
		return &Node{Number: n.Content(src)}
	case "quote":
		if n.NamedChildCount() > 0 {
			return &Node{Quote: convert(n.NamedChild(0), src)}
		}
		return &Node{Quote: &Node{}}
	default:
		list := make([]*Node, 0, n.NamedChildCount())
		for i := 0; i < int(n.NamedChildCount()); i++ {
			list = append(list, convert(n.NamedChild(i), src))
		}
		if len(list) > 0 {
			return &Node{List: list}
		}
		return &Node{Symbol: n.Content(src)}
	}
}
