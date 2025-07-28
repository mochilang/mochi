//go:build slow

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

// RawAST represents the Elixir AST as generic JSON.
type RawAST interface{}

// ParseAST parses Elixir source using the official parser and returns the raw AST in JSON form.
func ParseAST(src string) (RawAST, error) {
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
	var ast RawAST
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return ast, nil
}
