package c

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

// Node mirrors clang's JSON AST node structure.
type Node struct {
	Kind  string `json:"kind"`
	Name  string `json:"name"`
	Value string `json:"value"`
	Inner []Node `json:"inner"`
	Ref   *Node  `json:"referencedDecl"`
}

// Program holds the parsed C AST along with the original source.
type Program struct {
	Root   *Node
	Source string
}

// Parse uses clang to parse src and returns a Program.
func Parse(src string) (*Program, error) {
	root, err := runClangAST(src)
	if err != nil {
		return nil, err
	}
	return &Program{Root: root, Source: src}, nil
}

// DebugParse parses src and returns the raw AST node. Exposed for testing.
func DebugParse(src string) (*Node, error) { return runClangAST(src) }

func runClangAST(src string) (*Node, error) {
	cmd := exec.Command("clang", "-w", "-x", "c", "-", "-Xclang", "-ast-dump=json", "-fsyntax-only")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("clang failed: %v", err)
	}
	data := out.Bytes()
	var root Node
	if err := json.Unmarshal(data[jsonIndex(data):], &root); err != nil {
		return nil, err
	}
	return &root, nil
}

func jsonIndex(b []byte) int {
	for i, c := range b {
		if c == '{' {
			return i
		}
	}
	return 0
}
