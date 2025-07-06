package rust

import (
	"encoding/json"
	"fmt"
	"os"

	a2m "mochi/tools/any2mochi"
)

// ASTNode represents a node in the Rust syntax tree.
type ASTNode struct {
	Kind     string     `json:"kind"`
	Start    int        `json:"start"`
	End      int        `json:"end"`
	Children []*ASTNode `json:"children,omitempty"`
}

func toASTNode(n *node) *ASTNode {
	if n == nil {
		return nil
	}
	out := &ASTNode{Kind: n.kind, Start: n.start, End: n.end}
	for _, c := range n.children {
		out.Children = append(out.Children, toASTNode(c))
	}
	return out
}

func fromASTNode(r *ASTNode) *node {
	if r == nil {
		return nil
	}
	n := &node{kind: r.Kind, start: r.Start, end: r.End}
	for _, c := range r.Children {
		n.children = append(n.children, fromASTNode(c))
	}
	return n
}

// ParseAST parses the given Rust source code using rust-analyzer and returns
// the syntax tree as a ASTNode.
func ParseAST(src string) (*ASTNode, error) {
	ls := a2m.Servers["rust"]
	if ls.Command == "" {
		ls.Command = "rust-analyzer"
	}
	if err := a2m.EnsureServer(ls.Command); err != nil {
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
	return toASTNode(tree), nil
}

// ParseASTFile reads the Rust file and parses it to a ASTNode.
func ParseASTFile(path string) (*ASTNode, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ParseAST(string(data))
}

// MarshalAST writes the ASTNode as JSON.
func MarshalAST(ast *ASTNode) ([]byte, error) {
	return json.MarshalIndent(ast, "", "  ")
}
