package elixir

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

//go:embed parse.exs
var elixirParser string

// Meta stores position information for an AST node.
type Meta struct {
	Line   int `json:"line,omitempty"`
	Column int `json:"column,omitempty"`
}

// Node represents a single AST node produced by the Elixir parser.
type Node struct {
	Type     string `json:"type"`
	Meta     *Meta  `json:"meta,omitempty"`
	Args     []any  `json:"args,omitempty"`
	Ctx      string `json:"ctx,omitempty"`
	Key      *Node  `json:"key,omitempty"`
	Value    any    `json:"value,omitempty"`
	Elements []any  `json:"elements,omitempty"`
}

// Program holds the parsed AST of an Elixir file.
type Program struct {
	AST Node `json:"ast"`
}

// Inspect parses Elixir source code using the official parser and returns the Program.
func Inspect(src string) (*Program, error) {
	if _, err := exec.LookPath("elixir"); err != nil {
		return nil, fmt.Errorf("elixir not installed")
	}
	cmd := exec.Command("elixir", "-e", elixirParser)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("elixir parse error: %v: %s", err, errBuf.String())
	}
	var ast Node
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return &Program{AST: ast}, nil
}
