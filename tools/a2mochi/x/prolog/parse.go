//go:build slow

package prolog

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

//go:embed pl_ast.pl
var plScript string

// Program represents a parsed Prolog file.
type Program struct {
	Clauses []Clause `json:"clauses"`
}

// Clause represents a single predicate clause.
type Clause struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   string   `json:"body"`
}

// Parse parses Prolog source using swipl and returns the AST program.
func Parse(src string) (*Program, error) {
	if _, err := exec.LookPath("swipl"); err != nil {
		return nil, fmt.Errorf("swipl not installed")
	}
	tmp, err := os.CreateTemp("", "pl-*.pl")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(plScript); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	cmd := exec.Command("swipl", "-q", "-f", tmp.Name(), "-t", "main")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, fmt.Errorf("%s", strings.TrimSpace(errBuf.String()))
		}
		return nil, err
	}
	var prog Program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}
