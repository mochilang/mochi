package pl

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

// Program represents a parsed Prolog source file.
type Program struct {
	Clauses []Clause `json:"clauses"`
}

// Clause describes a single predicate clause.
type Clause struct {
	Name   string `json:"name"`
	Params []any  `json:"params"`
	Body   string `json:"body"`
	Start  int    `json:"start"`
	End    int    `json:"end"`
}

// Inspect parses the given Prolog source code using swipl and returns a Program describing its AST.
func Inspect(src string) (*Program, error) {
	exe := os.Getenv("SWIPL")
	if exe == "" {
		exe = "swipl"
	}
	if _, err := exec.LookPath(exe); err != nil {
		return nil, fmt.Errorf("%s not installed", exe)
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

	cmd := exec.Command(exe, "-q", "-f", tmp.Name(), "-t", "main")
	cmd.Stdin = strings.NewReader(src)
	var out, errBuf bytes.Buffer
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
