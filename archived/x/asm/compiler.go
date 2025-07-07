//go:build archived

package asm

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	ccode "mochi/archived/x/c"
	"mochi/parser"
	"mochi/types"
)

// Compiler generates assembly by delegating to the C backend and
// running the host C compiler with -S.
type Compiler struct {
	env *types.Env
}

func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile converts the program to C source and asks the C compiler
// to emit assembly.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	ccPath, err := ccode.EnsureCC()
	if err != nil {
		return nil, err
	}
	// Generate C source using the existing backend.
	cgen := ccode.New(c.env)
	csrc, err := cgen.Compile(prog)
	if err != nil {
		return nil, err
	}
	dir, err := os.MkdirTemp("", "mochi-asm")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(dir)
	cfile := filepath.Join(dir, "prog.c")
	sfile := filepath.Join(dir, "prog.s")
	if err := os.WriteFile(cfile, csrc, 0644); err != nil {
		return nil, err
	}
	cmd := exec.Command(ccPath, "-S", cfile, "-o", sfile)
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("cc error: %w\n%s", err, out)
	}
	asm, err := os.ReadFile(sfile)
	if err != nil {
		return nil, err
	}
	return asm, nil
}
