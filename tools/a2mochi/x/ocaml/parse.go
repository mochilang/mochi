//go:build slow

package ocaml

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// Program holds a simplified OCaml AST parsed via the helper Node script.
// Only a few fields are required for the limited examples used in the tests.
// Source retains the original OCaml code so the printer can include it.
type Program struct {
	Funcs  []Func      `json:"funcs"`
	Prints []PrintStmt `json:"prints"`
	Types  []Type      `json:"types"`
	Vars   []Var       `json:"vars"`
	Source string      `json:"-"`
}

type Func struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   string   `json:"body"`
}

type Var struct {
	Name    string `json:"name"`
	Expr    string `json:"expr"`
	Mutable bool   `json:"mutable"`
}

type PrintStmt struct {
	Expr string `json:"expr"`
}

type Type struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
}

type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Parse invokes the helper Node script to obtain a small OCaml AST.
// The script is implemented using tree-sitter and shipped with the repository,
// so no external tools are required other than Node.js.
func Parse(src string) (*Program, error) {
	src = stripGenerated(src)
	tmp, err := os.CreateTemp("", "ocaml-*.ml")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()

	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	script := filepath.Join(root, "tools", "a2mochi", "x", "ocaml", "ocaml_ast.js")
	cmd := exec.Command("node", script, tmp.Name())
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("node: %s", msg)
	}
	var prog Program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	prog.Source = src
	return &prog, nil
}

func stripGenerated(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 0 && strings.HasPrefix(strings.TrimSpace(lines[0]), "(* Generated") {
		i := 1
		for i < len(lines) && strings.TrimSpace(lines[i]) == "" {
			i++
		}
		lines = lines[i:]
	}
	return strings.Join(lines, "\n")
}
