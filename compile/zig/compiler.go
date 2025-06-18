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

// Compiler uses the C backend and zig cc to build a native binary.
type Compiler struct {
	env *types.Env
}

// New creates a new zig compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile returns a native binary that executes prog using zig cc.
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
	bin := filepath.Join(dir, "prog")
	cmd := exec.Command(zig, "cc", cfile, "-O3", "-o", bin)
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("zig cc error: %w\n%s", err, out)
	}
	return os.ReadFile(bin)
}
