package ex

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

// Program represents a parsed Elixir file.
type Program struct {
	AST interface{} `json:"ast"`
}

// Inspect parses Elixir source code and returns its AST.
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
	var ast interface{}
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return &Program{AST: ast}, nil
}
