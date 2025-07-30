//go:build slow

package zig

import (
	"bytes"
	"context"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	zigcode "mochi/compiler/x/zig"
)

//go:embed parse.zig
var zigParser string

// Token represents a Zig token.
type Token struct {
	Value string `json:"value"`
}

// Param represents a function parameter.
type Param struct {
	NameToken *Token `json:"name_token"`
	TypeExpr  Node   `json:"type_expr"`
}

// Node represents an AST node emitted by parse.zig.
type Node struct {
	Tag        string  `json:"tag"`
	NameToken  *Token  `json:"name_token,omitempty"`
	Params     []Param `json:"params,omitempty"`
	VisibToken *Token  `json:"visib_token,omitempty"`
	TypeNode   *Node   `json:"type_node,omitempty"`
	InitNode   *Node   `json:"init_node,omitempty"`
	MainToken  *Token  `json:"main_token,omitempty"`
	Members    []Node  `json:"members,omitempty"`
	TypeExpr   *Node   `json:"type_expr,omitempty"`
	ValueExpr  *Node   `json:"value_expr,omitempty"`
}

// Program holds the parsed nodes of a Zig source file.
type Program struct {
	Nodes []Node `json:"nodes"`
}

// Inspect parses Zig source code using the bundled parser.
func Inspect(src string) (*Program, error) {
	zigc, err := zigcode.EnsureZig()
	if err != nil {
		return nil, fmt.Errorf("zig not installed: %w", err)
	}

	dir, err := os.MkdirTemp("", "zig-ast-")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(dir)

	parserPath := filepath.Join(dir, "parse.zig")
	if err := os.WriteFile(parserPath, []byte(zigParser), 0644); err != nil {
		return nil, err
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, zigc, "run", parserPath)
	cmd.Stdin = strings.NewReader(src)
	var out, errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf.String())
		if msg != "" {
			return nil, fmt.Errorf("%v: %s", err, msg)
		}
		return nil, err
	}

	var nodes []Node
	if err := json.Unmarshal(out.Bytes(), &nodes); err != nil {
		return nil, err
	}
	return &Program{Nodes: nodes}, nil
}
