package rs

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

// Node represents a node in the Rust syntax tree.
type Node struct {
	Kind     string  `json:"kind"`
	Start    int     `json:"start"`
	End      int     `json:"end"`
	Children []*Node `json:"children,omitempty"`
}

// Program represents a parsed Rust source file.
type Program struct {
	File *Node `json:"file"`
}

// Inspect parses the given Rust source code using rust-analyzer and returns
// a Program describing its syntax tree.
func Inspect(src string) (*Program, error) {
	if _, err := exec.LookPath("rust-analyzer"); err != nil {
		return nil, fmt.Errorf("rust-analyzer not installed: %w", err)
	}
	out, err := runRustAnalyzerParse("rust-analyzer", src)
	if err != nil {
		return nil, err
	}
	tree := parseTree(out)
	return &Program{File: tree}, nil
}

func runRustAnalyzerParse(cmd, src string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	c := exec.CommandContext(ctx, cmd, "parse")
	c.Stdin = strings.NewReader(src)
	var stdout, stderr bytes.Buffer
	c.Stdout = &stdout
	c.Stderr = &stderr
	if err := c.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg != "" {
			return "", fmt.Errorf("%v: %s", err, msg)
		}
		return "", err
	}
	return stdout.String(), nil
}

type rawNode struct {
	kind     string
	start    int
	end      int
	children []*rawNode
}

func parseLine(line string) (indent int, kind string, start, end int, ok bool) {
	i := 0
	for i < len(line) && line[i] == ' ' {
		i++
	}
	indent = i / 2
	rest := line[i:]
	at := strings.IndexByte(rest, '@')
	if at < 0 {
		return
	}
	kind = rest[:at]
	rest = rest[at+1:]
	dots := strings.Index(rest, "..")
	if dots < 0 {
		return
	}
	s, err := strconv.Atoi(rest[:dots])
	if err != nil {
		return
	}
	start = s
	rest = rest[dots+2:]
	endStr := rest
	if sp := strings.IndexByte(rest, ' '); sp >= 0 {
		endStr = rest[:sp]
	}
	e, err := strconv.Atoi(endStr)
	if err != nil {
		return
	}
	end = e
	ok = true
	return
}

func parseTree(out string) *Node {
	root := &rawNode{kind: "ROOT"}
	stack := []*rawNode{root}
	for _, line := range strings.Split(strings.TrimSpace(out), "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		indent, kind, start, end, ok := parseLine(line)
		if !ok {
			continue
		}
		n := &rawNode{kind: kind, start: start, end: end}
		for len(stack) > indent+1 {
			stack = stack[:len(stack)-1]
		}
		parent := stack[len(stack)-1]
		parent.children = append(parent.children, n)
		stack = append(stack, n)
	}
	if len(root.children) > 0 {
		return toNode(root.children[0])
	}
	return toNode(root)
}

func toNode(n *rawNode) *Node {
	if n == nil {
		return nil
	}
	out := &Node{Kind: n.kind, Start: n.start, End: n.end}
	for _, c := range n.children {
		out.Children = append(out.Children, toNode(c))
	}
	return out
}

// MarshalJSON implements json.Marshaler for Program, to ensure stable output.
func (p *Program) MarshalJSON() ([]byte, error) {
	type Alias Program
	return json.Marshal(&struct{ *Alias }{Alias: (*Alias)(p)})
}
