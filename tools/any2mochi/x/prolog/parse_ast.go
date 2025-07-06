package prolog

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

type program struct {
	Clauses []clause `json:"clauses"`
}

type clause struct {
	Name      string   `json:"name"`
	Params    []string `json:"params"`
	Body      string   `json:"body"`
	Start     int      `json:"start"`
	End       int      `json:"end"`
	StartLine int
	StartCol  int
	EndLine   int
	EndCol    int
	Type      string
	Arity     int
}

func parseAST(src string) (*program, error) {
	_, file, _, _ := runtime.Caller(0)
	script := filepath.Join(filepath.Dir(file), "pl_ast.pl")
	cmd := exec.Command("swipl", "-q", "-f", script, "-t", "main")
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		if stderr.Len() > 0 {
			return nil, fmt.Errorf("%s", strings.TrimSpace(stderr.String()))
		}
		return nil, err
	}
	var prog program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	// compute line and column information from offsets
	lineOffsets := []int{0}
	for i, r := range src {
		if r == '\n' {
			lineOffsets = append(lineOffsets, i+1)
		}
	}
	for i, c := range prog.Clauses {
		l1, c1 := offsetToPos(lineOffsets, c.Start)
		l2, c2 := offsetToPos(lineOffsets, c.End)
		prog.Clauses[i].StartLine = l1
		prog.Clauses[i].StartCol = c1
		prog.Clauses[i].EndLine = l2
		prog.Clauses[i].EndCol = c2
		prog.Clauses[i].Arity = len(c.Params)
		if strings.TrimSpace(c.Body) == "true" {
			prog.Clauses[i].Type = "fact"
		} else {
			prog.Clauses[i].Type = "rule"
		}
	}
	return &prog, nil
}

func offsetToPos(lines []int, off int) (int, int) {
	for i := len(lines) - 1; i >= 0; i-- {
		if off >= lines[i] {
			return i + 1, off - lines[i] + 1
		}
	}
	return 1, off + 1
}

// ParseASTForTest is a test helper exposing parseAST.
func ParseASTForTest(src string) (*program, error) { return parseAST(src) }
