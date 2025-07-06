package any2mochi

import (
	"bytes"
	"encoding/json"
	"os/exec"
	"path/filepath"
	"runtime"
)

type plProgram struct {
	Clauses []plClause `json:"clauses"`
}

type plClause struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   string   `json:"body"`
}

func parsePrologAST(src string) (*plProgram, error) {
	_, file, _, _ := runtime.Caller(0)
	script := filepath.Join(filepath.Dir(file), "pl_ast.pl")
	cmd := exec.Command("swipl", "-q", "-f", script, "-t", "main")
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var prog plProgram
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}

// ParsePrologASTForTest is a test helper exposing parsePrologAST.
func ParsePrologASTForTest(src string) (*plProgram, error) { return parsePrologAST(src) }
