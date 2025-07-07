//go:build archived

package wasm

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	gocode "mochi/archived/go"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into a WebAssembly module.
//
// It works by first compiling the program to Go source using the
// Go compiler package and then invoking the Go toolchain to build
// the resulting source for the js/wasm target.
// The resulting WebAssembly binary bytes are returned.
// Toolchain specifies the compiler toolchain used to build the WebAssembly
// module.
type Toolchain string

const (
	// ToolchainGo uses the standard Go toolchain targeting js/wasm.
	ToolchainGo Toolchain = "go"
	// ToolchainTinyGo uses the TinyGo compiler.
	ToolchainTinyGo Toolchain = "tinygo"
)

// Option configures the Compiler.
type Option func(*Compiler)

// WithToolchain selects the toolchain used for compilation.
func WithToolchain(tc Toolchain) Option {
	return func(c *Compiler) { c.toolchain = tc }
}

type Compiler struct {
	env       *types.Env
	toolchain Toolchain
}

// New creates a new WASM compiler.
func New(env *types.Env, opts ...Option) *Compiler {
	c := &Compiler{env: env, toolchain: ToolchainGo}
	for _, opt := range opts {
		opt(c)
	}
	return c
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
	var cmd *exec.Cmd
	switch c.toolchain {
	case ToolchainTinyGo:
		cmd = exec.Command("tinygo", "build", "-o", outPath, "-target", "wasm", srcPath)
	default:
		cmd = exec.Command("go", "build", "-o", outPath, srcPath)
		cmd.Env = append(os.Environ(), "GOOS=js", "GOARCH=wasm")
	}
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("%s build failed: %w\n%s", c.toolchain, err, out)
	}
	data, err := os.ReadFile(outPath)
	if err != nil {
		return nil, err
	}
	return data, nil
}
