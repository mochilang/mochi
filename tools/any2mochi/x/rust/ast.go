package rust

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	any2mochi "mochi/tools/any2mochi"
)

// RustASTNode represents a node in the Rust syntax tree.
type ASTNode struct {
	ID          int        `json:"id"`
	Kind        string     `json:"kind"`
	Start       int        `json:"start"`
	End         int        `json:"end"`
	Children    []*ASTNode `json:"children,omitempty"`
	StartLine   int        `json:"startLine,omitempty"`
	StartColumn int        `json:"startColumn,omitempty"`
	EndLine     int        `json:"endLine,omitempty"`
	EndColumn   int        `json:"endColumn,omitempty"`
	Parent      string     `json:"parent,omitempty"`
	Text        string     `json:"text,omitempty"`
	Snippet     string     `json:"snippet,omitempty"`
}

func position(src string, off int) (line, col int) {
	line = 1
	col = 1
	for i := 0; i < len(src) && i < off; i++ {
		if src[i] == '\n' {
			line++
			col = 1
		} else {
			col++
		}
	}
	return
}

func toASTNode(src string, n *node, parent string, id *int) *ASTNode {
	if n == nil {
		return nil
	}
	sl, sc := position(src, n.start)
	el, ec := position(src, n.end)
	*id = *id + 1
	out := &ASTNode{
		ID:          *id,
		Kind:        n.kind,
		Start:       n.start,
		End:         n.end,
		StartLine:   sl,
		StartColumn: sc,
		EndLine:     el,
		EndColumn:   ec,
		Parent:      parent,
		Text:        strings.TrimSpace(src[n.start:n.end]),
		Snippet:     lineSnippet(src, sl),
	}
	for _, c := range n.children {
		out.Children = append(out.Children, toASTNode(src, c, n.kind, id))
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
// the syntax tree as an ASTNode.
func ParseAST(src string) (*ASTNode, error) {
	ls := any2mochi.Servers["rust"]
	if ls.Command == "" {
		ls.Command = "rust-analyzer"
	}
	if err := any2mochi.EnsureServer(ls.Command); err != nil {
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
	id := 0
	return toASTNode(src, tree, "", &id), nil
}

// ParseASTFile reads the Rust file and parses it to an ASTNode.
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

func lineSnippet(src string, line int) string {
	lines := strings.Split(src, "\n")
	if line-1 < 0 || line-1 >= len(lines) {
		return ""
	}
	return strings.TrimSpace(lines[line-1])
}
