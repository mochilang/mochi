package prolog

import (
	"bytes"
	"encoding/json"
	"os/exec"
	"path/filepath"
	"runtime"
)

type program struct {
	Clauses []clause `json:"clauses"`
}

type clause struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   string   `json:"body"`
}

func parseAST(src string) (*program, error) {
	_, file, _, _ := runtime.Caller(0)
	script := filepath.Join(filepath.Dir(file), "pl_ast.pl")
	cmd := exec.Command("swipl", "-q", "-f", script, "-t", "main")
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var prog program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}

// ParseASTForTest is a test helper exposing parseAST.
func ParseASTForTest(src string) (*program, error) { return parseAST(src) }
