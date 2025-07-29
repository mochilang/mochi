//go:build slow

package ts

import (
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
)

//go:embed parse.ts
var tsParser string

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

// Node is a single top-level statement from the TypeScript AST.
type Node struct {
	Kind     string   `json:"kind"`
	Name     string   `json:"name"`
	Node     string   `json:"node,omitempty"`
	Params   []Param  `json:"params,omitempty"`
	Ret      string   `json:"ret,omitempty"`
	Body     string   `json:"body,omitempty"`
	Value    string   `json:"value,omitempty"`
	Fields   []Field  `json:"fields,omitempty"`
	Alias    string   `json:"alias,omitempty"`
	Variants []string `json:"variants,omitempty"`
	Expr     string   `json:"expr,omitempty"`
	Iter     string   `json:"iter,omitempty"`
	List     string   `json:"list,omitempty"`
	StartVal string   `json:"startVal,omitempty"`
	EndVal   string   `json:"endVal,omitempty"`
	Cond     string   `json:"cond,omitempty"`
}

// Program is the result of parsing a TypeScript source file.
type Program struct {
	Nodes  []Node `json:"nodes"`
	Source string `json:"-"`
}

// Parse runs the embedded TypeScript parser and returns a Program.
func Parse(src string) (*Program, error) {
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

	script, err := os.CreateTemp("", "ts-parse-*.ts")
	if err != nil {
		os.Remove(tmp.Name())
		return nil, err
	}
	if _, err := script.WriteString(tsParser); err != nil {
		os.Remove(tmp.Name())
		os.Remove(script.Name())
		return nil, err
	}
	script.Close()
	defer os.Remove(script.Name())

	cmd := exec.Command("deno", "run", "--quiet", "--allow-read", "--allow-env", "--node-modules-dir=auto", script.Name(), tmp.Name())
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
