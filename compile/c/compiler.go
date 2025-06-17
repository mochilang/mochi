package ccode

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	pycode "mochi/compile/py"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into C source code by first compiling to
// Python and then using Cython's --embed mode to generate C code embedding
// the Python interpreter.
type Compiler struct {
	env *types.Env
}

// New creates a new C compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile returns C source code that executes prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	dir, err := os.MkdirTemp("", "mochi-c-")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(dir)

	pyPath := filepath.Join(dir, "prog.py")
	cPath := filepath.Join(dir, "prog.c")

	pyCode, err := pycode.New(c.env).Compile(prog)
	if err != nil {
		return nil, err
	}
	if err := os.WriteFile(pyPath, pyCode, 0644); err != nil {
		return nil, err
	}

	cmd := exec.Command("cython", pyPath, "-3", "--embed", "-o", cPath)
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("cython failed: %w\n%s", err, out)
	}

	data, err := os.ReadFile(cPath)
	if err != nil {
		return nil, err
	}
	return data, nil
}
