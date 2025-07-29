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

// Node represents the simplified Java AST produced by the helper parser.
type Node struct {
	Body []Stmt `json:"body"`
}

// Stmt describes a statement in the simplified AST.
type Stmt struct {
	Kind  string `json:"kind"`
	Name  string `json:"name,omitempty"`
	Expr  *Expr  `json:"expr,omitempty"`
	Cond  *Expr  `json:"cond,omitempty"`
	Start *Expr  `json:"start,omitempty"`
	End   *Expr  `json:"end,omitempty"`
	Body  []Stmt `json:"body,omitempty"`
	Then  []Stmt `json:"then,omitempty"`
	Else  []Stmt `json:"else,omitempty"`
}

// Expr describes an expression in the simplified AST.
type Expr struct {
	Kind   string `json:"kind"`
	Value  string `json:"value,omitempty"`
	Name   string `json:"name,omitempty"`
	Left   *Expr  `json:"left,omitempty"`
	Right  *Expr  `json:"right,omitempty"`
	Target *Expr  `json:"target,omitempty"`
	Args   []Expr `json:"args,omitempty"`
	Expr   *Expr  `json:"expr,omitempty"`
	Cond   *Expr  `json:"cond,omitempty"`
	Then   *Expr  `json:"then,omitempty"`
	Else   *Expr  `json:"else,omitempty"`
	Elems  []Expr `json:"elems,omitempty"`
	Op     string `json:"op,omitempty"`
}

//go:embed internal/AstJson.java
var astJSON []byte

var (
	compileOnce sync.Once
	compileErr  error
	classDir    string
)

// ensureCompiled compiles the Java helper parser if needed.
func ensureCompiled() error {
	compileOnce.Do(func() {
		tmp, err := os.MkdirTemp("", "a2mochi-java-*")
		if err != nil {
			compileErr = err
			return
		}
		src := filepath.Join(tmp, "AstJson.java")
		if err := os.WriteFile(src, astJSON, 0o644); err != nil {
			compileErr = err
			return
		}
		classDir = filepath.Join(tmp, "bin")
		os.MkdirAll(classDir, 0o755)
		cmd := exec.Command("javac", "-d", classDir, src)
		if out, err := cmd.CombinedOutput(); err != nil {
			compileErr = fmt.Errorf("javac: %v: %s", err, out)
		}
	})
	return compileErr
}

// Parse converts Java source into a Node by invoking the helper Java parser.
func Parse(src string) (*Node, error) {
	tmp, err := os.CreateTemp("", "a2mochi-java-*.java")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()
	return ParseFile(tmp.Name())
}

// ParseFile parses a Java file and returns a Node.
func ParseFile(path string) (*Node, error) {
	if err := ensureCompiled(); err != nil {
		return nil, err
	}
	cmd := exec.Command("java", "-cp", classDir, "internal.AstJson", path)
	out, err := cmd.Output()
	if err != nil {
		if ee, ok := err.(*exec.ExitError); ok {
			return nil, fmt.Errorf("java error: %s", string(ee.Stderr))
		}
		return nil, err
	}
	var n Node
	if err := json.Unmarshal(out, &n); err != nil {
		return nil, err
	}
	return &n, nil
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}
