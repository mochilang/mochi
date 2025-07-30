//go:build slow

package java

import (
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
)

// Program represents the parsed structure of a Java source file.
type Program struct {
	Body []Stmt `json:"body"`
}

// Stmt represents a statement node in the simplified AST.
type Stmt struct {
	Kind   string  `json:"kind"`
	Name   string  `json:"name,omitempty"`
	Type   string  `json:"type,omitempty"`
	Target *Expr   `json:"target,omitempty"`
	Expr   *Expr   `json:"expr,omitempty"`
	Cond   *Expr   `json:"cond,omitempty"`
	Start  *Expr   `json:"start,omitempty"`
	End    *Expr   `json:"end,omitempty"`
	Body   []Stmt  `json:"body,omitempty"`
	Then   []Stmt  `json:"then,omitempty"`
	Else   []Stmt  `json:"else,omitempty"`
	Params []Param `json:"params,omitempty"`
}

// Param represents a function parameter.
type Param struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

// Expr represents an expression node in the simplified AST.
type Expr struct {
	Kind   string  `json:"kind"`
	Value  string  `json:"value,omitempty"`
	Name   string  `json:"name,omitempty"`
	Text   string  `json:"text,omitempty"`
	Left   *Expr   `json:"left,omitempty"`
	Right  *Expr   `json:"right,omitempty"`
	Target *Expr   `json:"target,omitempty"`
	Args   []Expr  `json:"args,omitempty"`
	Expr   *Expr   `json:"expr,omitempty"`
	Index  *Expr   `json:"index,omitempty"`
	Cond   *Expr   `json:"cond,omitempty"`
	Then   *Expr   `json:"then,omitempty"`
	Else   *Expr   `json:"else,omitempty"`
	Elems  []Expr  `json:"elems,omitempty"`
	Params []Param `json:"params,omitempty"`
	Body   []Stmt  `json:"body,omitempty"`
	Op     string  `json:"op,omitempty"`
}

//go:embed internal/AstJson.java
var astSource []byte

var (
	compileOnce sync.Once
	compileErr  error
	classDir    string
)

// ensureCompiled compiles the helper Java parser once.
func ensureCompiled() error {
	compileOnce.Do(func() {
		tmp, err := os.MkdirTemp("", "jsonast-java-*")
		if err != nil {
			compileErr = err
			return
		}
		src := filepath.Join(tmp, "AstJson.java")
		if err := os.WriteFile(src, astSource, 0o644); err != nil {
			compileErr = err
			return
		}
		classDir = filepath.Join(tmp, "bin")
		os.MkdirAll(classDir, 0o755)
		cmd := exec.Command("javac", "-encoding", "UTF-8", "-d", classDir, src)
		if out, err := cmd.CombinedOutput(); err != nil {
			compileErr = fmt.Errorf("javac: %v: %s", err, out)
		}
	})
	return compileErr
}

// Inspect parses src using the embedded Java parser and returns a Program.
func Inspect(src string) (*Program, error) {
	tmp, err := os.CreateTemp("", "jsonast-java-*.java")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	if err := tmp.Close(); err != nil {
		return nil, err
	}
	return inspectFile(tmp.Name())
}

func inspectFile(path string) (*Program, error) {
	if err := ensureCompiled(); err != nil {
		return nil, err
	}
	cmd := exec.Command("java", "-cp", classDir, "internal.AstJson", path)
	out, err := cmd.Output()
	if err != nil {
		if ee, ok := err.(*exec.ExitError); ok {
			return nil, fmt.Errorf("java error: %s", ee.Stderr)
		}
		return nil, err
	}
	var prog Program
	if err := json.Unmarshal(out, &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}
