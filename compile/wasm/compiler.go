package wasm

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	gocode "mochi/compile/go"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into a WebAssembly module.
//
// It works by first compiling the program to Go source using the
// Go compiler package and then invoking the Go toolchain to build
// the resulting source for the js/wasm target.
// The resulting WebAssembly binary bytes are returned.
type Compiler struct {
	env *types.Env
}

// New creates a new WASM compiler.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

// Compile returns a WebAssembly binary that executes prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	dir, err := os.MkdirTemp("", "mochi-wasm-")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(dir)

	srcPath := filepath.Join(dir, "main.go")
	code, err := gocode.New(c.env).Compile(prog)
	if err != nil {
		return nil, err
	}
	if err := os.WriteFile(srcPath, code, 0644); err != nil {
		return nil, err
	}

	outPath := filepath.Join(dir, "prog.wasm")
	cmd := exec.Command("go", "build", "-o", outPath, srcPath)
	cmd.Env = append(os.Environ(), "GOOS=js", "GOARCH=wasm")
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("go build failed: %w\n%s", err, out)
	}
	data, err := os.ReadFile(outPath)
	if err != nil {
		return nil, err
	}
	return data, nil
}
