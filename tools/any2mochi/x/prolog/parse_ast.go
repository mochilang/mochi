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
	Start  int      `json:"start"`
	End    int      `json:"end"`
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
	// compute line numbers from character offsets
	lineOffsets := []int{0}
	for i, r := range src {
		if r == '\n' {
			lineOffsets = append(lineOffsets, i+1)
		}
	}
	for i, c := range prog.Clauses {
		prog.Clauses[i].Start = offsetToLine(lineOffsets, c.Start)
		prog.Clauses[i].End = offsetToLine(lineOffsets, c.End)
	}
	return &prog, nil
}

func offsetToLine(lines []int, off int) int {
	for i := len(lines) - 1; i >= 0; i-- {
		if off >= lines[i] {
			return i + 1
		}
	}
	return 1
}

// ParseASTForTest is a test helper exposing parseAST.
func ParseASTForTest(src string) (*program, error) { return parseAST(src) }
