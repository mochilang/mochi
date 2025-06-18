package zigcode

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	ccode "mochi/compile/c"
	"mochi/parser"
	"mochi/types"
)

// Compiler uses the C backend to generate Zig source code via `zig translate-c`.
type Compiler struct {
	env *types.Env
}

// New creates a new zig compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile returns Zig source code that implements prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	zig, err := EnsureZig()
	if err != nil {
		return nil, err
	}
	csrc, err := ccode.New(c.env).Compile(prog)
	if err != nil {
		return nil, err
	}
	dir, err := os.MkdirTemp("", "mochi-zig-")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(dir)

	cfile := filepath.Join(dir, "prog.c")
	if err := os.WriteFile(cfile, csrc, 0644); err != nil {
		return nil, err
	}
	zfile := filepath.Join(dir, "prog.zig")
	cmd := exec.Command(zig, "translate-c", cfile)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("zig translate-c error: %w\n%s", err, out)
	}
	if err := os.WriteFile(zfile, out, 0644); err != nil {
		return nil, err
	}
	return out, nil
}
