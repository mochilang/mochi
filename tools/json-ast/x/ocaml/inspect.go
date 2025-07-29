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

// Program represents a parsed OCaml source file as produced by the
// helper Node script. It mirrors the structure of ocaml_ast.js.
type Program struct {
	Funcs  []Func  `json:"funcs"`
	Prints []Print `json:"prints"`
	Types  []Type  `json:"types"`
	Vars   []Var   `json:"vars"`
}

type Func struct {
	Name    string   `json:"name"`
	Params  []string `json:"params"`
	Body    string   `json:"body"`
	Line    int      `json:"line"`
	Col     int      `json:"col"`
	EndLine int      `json:"endLine"`
	EndCol  int      `json:"endCol"`
	Snippet string   `json:"snippet"`
}

type Var struct {
	Name    string `json:"name"`
	Expr    string `json:"expr"`
	Mutable bool   `json:"mutable"`
	Line    int    `json:"line"`
	Col     int    `json:"col"`
	EndLine int    `json:"endLine"`
	EndCol  int    `json:"endCol"`
	Snippet string `json:"snippet"`
}

type Print struct {
	Expr    string `json:"expr"`
	Line    int    `json:"line"`
	Col     int    `json:"col"`
	EndLine int    `json:"endLine"`
	EndCol  int    `json:"endCol"`
	Snippet string `json:"snippet"`
}

type Type struct {
	Name    string  `json:"name"`
	Fields  []Field `json:"fields"`
	Line    int     `json:"line"`
	Col     int     `json:"col"`
	EndLine int     `json:"endLine"`
	EndCol  int     `json:"endCol"`
	Snippet string  `json:"snippet"`
}

type Field struct {
	Name    string `json:"name"`
	Type    string `json:"type"`
	Line    int    `json:"line"`
	Col     int    `json:"col"`
	EndLine int    `json:"endLine"`
	EndCol  int    `json:"endCol"`
	Snippet string `json:"snippet"`
}

// Inspect parses the given OCaml source code and returns a Program
// describing its structure using the bundled tree-sitter parser.
func Inspect(src string) (*Program, error) {
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
	var out, errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("node: %s", msg)
	}
	var prog Program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}
