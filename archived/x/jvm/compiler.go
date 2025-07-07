//go:build archived

package jvm

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	gocode "mochi/archived/go"
	"mochi/parser"
	"mochi/types"
)

// Option configures the Compiler.
type Option func(*Compiler)

// WithGomobile sets the gomobile command used for building the jar.
func WithGomobile(cmd string) Option {
	return func(c *Compiler) { c.gomobile = cmd }
}

// Compiler translates a Mochi AST into a JVM jar using gomobile.
type Compiler struct {
	env      *types.Env
	gomobile string
}

// New creates a new JVM compiler.
func New(env *types.Env, opts ...Option) *Compiler {
	c := &Compiler{env: env, gomobile: "gomobile"}
	for _, opt := range opts {
		opt(c)
	}
	return c
}

// Compile returns a jar file that executes prog using gomobile bind.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	dir, err := os.MkdirTemp("", "mochi-jvm-")
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

	// gomobile bind requires a package directory
	pkgDir := filepath.Join(dir, "pkg")
	if err := os.Mkdir(pkgDir, 0755); err != nil {
		return nil, err
	}
	if err := os.Rename(srcPath, filepath.Join(pkgDir, "main.go")); err != nil {
		return nil, err
	}

	jarPath := filepath.Join(dir, "prog.jar")
	cmd := exec.Command(c.gomobile, "bind", "-target=java", "-o", jarPath, pkgDir)
	cmd.Env = append(os.Environ(), "GO111MODULE=on")
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("gomobile bind failed: %w\n%s", err, out)
	}

	data, err := os.ReadFile(jarPath)
	if err != nil {
		return nil, err
	}
	return data, nil
}
