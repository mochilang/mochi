package ts

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// Param represents a function parameter.
type Param struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

// Field represents a struct or class field.
type Field struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

// Node is a single AST node from the TypeScript parser.
type Node struct {
	Kind      string   `json:"kind"`
	Name      string   `json:"name"`
	Node      string   `json:"node,omitempty"`
	Params    []Param  `json:"params,omitempty"`
	Ret       string   `json:"ret,omitempty"`
	Body      string   `json:"body,omitempty"`
	Value     string   `json:"value,omitempty"`
	Fields    []Field  `json:"fields,omitempty"`
	Alias     string   `json:"alias,omitempty"`
	Variants  []string `json:"variants,omitempty"`
	Expr      string   `json:"expr,omitempty"`
	Iter      string   `json:"iter,omitempty"`
	List      string   `json:"list,omitempty"`
	StartVal  string   `json:"startVal,omitempty"`
	EndVal    string   `json:"endVal,omitempty"`
	Cond      string   `json:"cond,omitempty"`
	Else      string   `json:"else,omitempty"`
	BodyNodes []Node   `json:"bodyNodes,omitempty"`
	ElseNodes []Node   `json:"elseNodes,omitempty"`
	Start     int      `json:"start,omitempty"`
	StartCol  int      `json:"startCol,omitempty"`
	End       int      `json:"end,omitempty"`
	EndCol    int      `json:"endCol,omitempty"`
	Snippet   string   `json:"snippet,omitempty"`
	StartOff  int      `json:"startOff,omitempty"`
	EndOff    int      `json:"endOff,omitempty"`
	Doc       string   `json:"doc,omitempty"`
}

// Program is the result of parsing a TypeScript source file.
type Program struct {
	Nodes  []Node `json:"nodes"`
	Source string `json:"-"`
}

// Inspect parses src using the embedded TypeScript parser and returns a Program.
func Inspect(src string) (*Program, error) {
	if _, err := exec.LookPath("deno"); err != nil {
		return nil, fmt.Errorf("deno not installed")
	}

	tmp, err := os.CreateTemp("", "ts-src-*.ts")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	root, err := repoRoot()
	if err != nil {
		os.Remove(tmp.Name())
		return nil, err
	}
	scriptPath := filepath.Join(root, "tools", "a2mochi", "x", "ts", "parse.ts")

	cmd := exec.Command("deno", "run", "--quiet", "--allow-read", "--allow-env", "--node-modules-dir=auto", scriptPath, tmp.Name())
	cmd.Dir = filepath.Join(root, "tools", "a2mochi", "x", "ts")
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("deno error: %w\n%s", err, out)
	}

	var nodes []Node
	if err := json.Unmarshal(out, &nodes); err != nil {
		return nil, err
	}
	return &Program{Nodes: nodes, Source: src}, nil
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
