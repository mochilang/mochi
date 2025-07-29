//go:build slow

package rust

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// RustASTNode represents a node in the Rust syntax tree.
type ASTNode struct {
	ID          int        `json:"id"`
	Kind        string     `json:"kind"`
	Start       int        `json:"start"`
	End         int        `json:"end"`
	Length      int        `json:"length,omitempty"`
	Children    []*ASTNode `json:"children,omitempty"`
	StartLine   int        `json:"startLine,omitempty"`
	StartColumn int        `json:"startColumn,omitempty"`
	EndLine     int        `json:"endLine,omitempty"`
	EndColumn   int        `json:"endColumn,omitempty"`
	Parent      string     `json:"parent,omitempty"`
	ParentID    int        `json:"parentID,omitempty"`
	Text        string     `json:"text,omitempty"`
	Snippet     string     `json:"snippet,omitempty"`
	Depth       int        `json:"depth,omitempty"`
	Index       int        `json:"index,omitempty"`
}

// Program bundles the Rust AST with its original source code.
type Program struct {
	Source string   `json:"source"`
	AST    *ASTNode `json:"ast"`
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

func toASTNode(src string, n *node, parent string, parentID int, id *int, depth int, index int) *ASTNode {
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
		Length:      n.end - n.start,
		StartLine:   sl,
		StartColumn: sc,
		EndLine:     el,
		EndColumn:   ec,
		Parent:      parent,
		ParentID:    parentID,
		Text:        strings.TrimSpace(src[n.start:n.end]),
		Snippet:     lineSnippet(src, sl),
		Depth:       depth,
		Index:       index,
	}
	for i, c := range n.children {
		out.Children = append(out.Children, toASTNode(src, c, n.kind, out.ID, id, depth+1, i))
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
	cmd := os.Getenv("RUST_ANALYZER")
	if cmd == "" {
		cmd = "rust-analyzer"
	}
	if _, err := exec.LookPath(cmd); err != nil {
		return nil, fmt.Errorf("rust-analyzer not installed")
	}
	if err := exec.Command(cmd, "--version").Run(); err != nil {
		return nil, fmt.Errorf("rust-analyzer not installed")
	}
	ast, err := runRustAnalyzerParse(cmd, src)
	if err != nil {
		return nil, err
	}
	tree := parseTree(ast)
	if tree == nil {
		return nil, fmt.Errorf("parse failed")
	}
	id := 0
	return toASTNode(src, tree, "", -1, &id, 0, 0), nil
}

// Parse parses Rust source code using rust-analyzer and returns a Program.
func Parse(src string) (*Program, error) {
	ast, err := ParseAST(src)
	if err != nil {
		return nil, err
	}
	return &Program{Source: src, AST: ast}, nil
}

// ParseBytes parses Rust source code from a byte slice.
func ParseBytes(data []byte) (*Program, error) {
	return Parse(string(data))
}

// ParseFile reads the Rust file and parses it.
func ParseFile(path string) (*Program, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Parse(string(data))
}

// ParseASTFile reads the Rust file and parses it to an ASTNode.
func ParseASTFile(path string) (*ASTNode, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ParseAST(string(data))
}

// ParseASTBytes parses Rust source bytes into an ASTNode.
func ParseASTBytes(data []byte) (*ASTNode, error) {
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

func runRustAnalyzerParse(cmd, src string) (string, error) {
	// allow a longer timeout to handle larger inputs when parsing without
	// crashing the external process
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()
	c := exec.CommandContext(ctx, cmd, "parse")
	c.Stdin = strings.NewReader(src)
	var out, stderr bytes.Buffer
	c.Stdout = &out
	c.Stderr = &stderr
	if err := c.Run(); err != nil {
		if msg := strings.TrimSpace(stderr.String()); msg != "" {
			return "", fmt.Errorf("%v: %s", err, msg)
		}
		return "", err
	}
	return out.String(), nil
}
