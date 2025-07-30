package ts

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// Node represents a node in the TypeScript AST.
type Node struct {
	Kind     string      `json:"kind"`
	Name     string      `json:"name,omitempty"`
	Value    interface{} `json:"value,omitempty"`
	Start    int         `json:"start"`
	StartCol int         `json:"startCol"`
	End      int         `json:"end"`
	EndCol   int         `json:"endCol"`
	Children []Node      `json:"children,omitempty"`
}

// Program represents a parsed TypeScript source file.
type Program struct {
	Root Node `json:"root"`
}

// Inspect parses the given TypeScript source code using the official parser
// and returns its Program structure.
func Inspect(src string) (*Program, error) {
	tscBin, err := exec.LookPath("tsc")
	if err != nil {
		return nil, fmt.Errorf("tsc not installed")
	}
	tsLib := filepath.Join(filepath.Dir(tscBin), "..", "lib", "node_modules", "typescript", "lib", "typescript.js")
	tsLib, _ = filepath.Abs(tsLib)

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
		return nil, err
	}
	script := filepath.Join(root, "tools", "json-ast", "x", "ts", "parse.js")

	cmd := exec.Command("node", script, tmp.Name())
	cmd.Env = append(os.Environ(), "TS_LIB="+tsLib)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("node error: %w\n%s", err, out)
	}

	var n Node
	if err := json.Unmarshal(out, &n); err != nil {
		return nil, err
	}
	return &Program{Root: n}, nil
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
