package clj

import (
	sitter "github.com/smacker/go-tree-sitter"
	"strconv"
)

// Node represents a Clojure form.
type Node struct {
	Sym    string     `json:"sym,omitempty"`
	Kw     string     `json:"kw,omitempty"`
	Str    string     `json:"str,omitempty"`
	Num    *float64   `json:"num,omitempty"`
	Bool   *bool      `json:"bool,omitempty"`
	Nil    bool       `json:"nil,omitempty"`
	List   []*Node    `json:"list,omitempty"`
	Vector []*Node    `json:"vector,omitempty"`
	Map    []MapEntry `json:"map,omitempty"`
}

// MapEntry is a key/value pair within a map literal.
type MapEntry struct {
	Key *Node `json:"key"`
	Val *Node `json:"val"`
}

// convertNode converts a tree-sitter node to our Node structure.
func convertNode(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	switch n.Type() {
	case "sym_lit":
		return &Node{Sym: n.Content(src)}
	case "kwd_lit":
		return &Node{Kw: n.Content(src)}
	case "str_lit":
		text := n.Content(src)
		if len(text) >= 2 && text[0] == '"' && text[len(text)-1] == '"' {
			text = text[1 : len(text)-1]
		}
		return &Node{Str: text}
	case "num_lit":
		if v, err := strconv.ParseFloat(n.Content(src), 64); err == nil {
			return &Node{Num: &v}
		}
	case "bool_lit":
		b := n.Content(src) == "true"
		return &Node{Bool: &b}
	case "nil_lit":
		return &Node{Nil: true}
	case "list_lit":
		node := &Node{}
		for i := 0; i < int(n.NamedChildCount()); i++ {
			node.List = append(node.List, convertNode(n.NamedChild(i), src))
		}
		return node
	case "vec_lit":
		node := &Node{}
		for i := 0; i < int(n.NamedChildCount()); i++ {
			node.Vector = append(node.Vector, convertNode(n.NamedChild(i), src))
		}
		return node
	case "map_lit":
		node := &Node{}
		count := int(n.NamedChildCount())
		for i := 0; i+1 < count; i += 2 {
			k := convertNode(n.NamedChild(i), src)
			v := convertNode(n.NamedChild(i+1), src)
			node.Map = append(node.Map, MapEntry{Key: k, Val: v})
		}
		return node
	}
	// Fallback: convert children as list
	if n.NamedChildCount() > 0 {
		node := &Node{}
		for i := 0; i < int(n.NamedChildCount()); i++ {
			node.List = append(node.List, convertNode(n.NamedChild(i), src))
		}
		return node
	}
	return &Node{Sym: n.Content(src)}
}

// Program is a parsed Clojure source file.
type Program struct {
	Forms []*Node `json:"forms"`
}
