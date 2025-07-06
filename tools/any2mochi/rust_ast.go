package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
)

// RustASTNode represents a node in the Rust syntax tree.
type RustASTNode struct {
	Kind     string         `json:"kind"`
	Start    int            `json:"start"`
	End      int            `json:"end"`
	Children []*RustASTNode `json:"children,omitempty"`
}

func toRustASTNode(n *node) *RustASTNode {
	if n == nil {
		return nil
	}
	out := &RustASTNode{Kind: n.kind, Start: n.start, End: n.end}
	for _, c := range n.children {
		out.Children = append(out.Children, toRustASTNode(c))
	}
	return out
}

func fromRustASTNode(r *RustASTNode) *node {
	if r == nil {
		return nil
	}
	n := &node{kind: r.Kind, start: r.Start, end: r.End}
	for _, c := range r.Children {
		n.children = append(n.children, fromRustASTNode(c))
	}
	return n
}

// ParseRustAST parses the given Rust source code using rust-analyzer and returns
// the syntax tree as a RustASTNode.
func ParseRustAST(src string) (*RustASTNode, error) {
	ls := Servers["rust"]
	if ls.Command == "" {
		ls.Command = "rust-analyzer"
	}
	if err := EnsureServer(ls.Command); err != nil {
		return nil, err
	}
	ast, err := runRustAnalyzerParse(ls.Command, src)
	if err != nil {
		return nil, err
	}
	tree := parseTree(ast)
	if tree == nil {
		return nil, fmt.Errorf("parse failed")
	}
	return toRustASTNode(tree), nil
}

// ParseRustASTFile reads the Rust file and parses it to a RustASTNode.
func ParseRustASTFile(path string) (*RustASTNode, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ParseRustAST(string(data))
}

// MarshalRustAST writes the RustASTNode as JSON.
func MarshalRustAST(ast *RustASTNode) ([]byte, error) {
	return json.MarshalIndent(ast, "", "  ")
}
