package clj

import (
	"strconv"
	"strings"

	sitter "github.com/smacker/go-tree-sitter"
)

// Node represents a Clojure form in a type-safe structure.
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

// MapEntry represents a key/value pair within a map literal.
type MapEntry struct {
	Key *Node `json:"key"`
	Val *Node `json:"val"`
}

// toAST converts a tree-sitter node into a Node structure.
func toAST(n *sitter.Node, src []byte) *Node {
	if n == nil {
		return nil
	}
	switch n.Type() {
	case "sym_lit", "symbol":
		return &Node{Sym: n.Content(src)}
	case "kwd_lit", "keyword":
		return &Node{Kw: n.Content(src)}
	case "str_lit", "string":
		s := n.Content(src)
		s = strings.TrimPrefix(s, "\"")
		s = strings.TrimSuffix(s, "\"")
		return &Node{Str: s}
	case "char_lit":
		return &Node{Str: n.Content(src)}
	case "num_lit", "number":
		if f, err := strconv.ParseFloat(n.Content(src), 64); err == nil {
			return &Node{Num: &f}
		}
	case "bool_lit", "boolean":
		val := n.Content(src)
		if val == "true" {
			b := true
			return &Node{Bool: &b}
		}
		if val == "false" {
			b := false
			return &Node{Bool: &b}
		}
	case "nil_lit", "nil":
		return &Node{Nil: true}
	case "list_lit", "list":
		node := &Node{}
		for i := 0; i < int(n.NamedChildCount()); i++ {
			child := n.NamedChild(i)
			node.List = append(node.List, toAST(child, src))
		}
		return node
	case "vec_lit", "vector":
		node := &Node{}
		for i := 0; i < int(n.NamedChildCount()); i++ {
			child := n.NamedChild(i)
			node.Vector = append(node.Vector, toAST(child, src))
		}
		return node
	case "map_lit", "map":
		node := &Node{}
		var key *Node
		for i := 0; i < int(n.NamedChildCount()); i++ {
			child := n.NamedChild(i)
			if key == nil {
				key = toAST(child, src)
			} else {
				node.Map = append(node.Map, MapEntry{Key: key, Val: toAST(child, src)})
				key = nil
			}
		}
		return node
	}
	// Fallback: treat as symbol
	return &Node{Sym: n.Content(src)}
}
