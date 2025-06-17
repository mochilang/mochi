package ccode

import (
	"archive/zip"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	gocode "mochi/compile/go"
	"mochi/parser"
	"mochi/types"
)

// BuildMode controls how the Go tool produces C-compatible output.
// It maps to the go build -buildmode flag.
type BuildMode string

const (
	// BuildModeArchive produces a static library (.a) and header (.h).
	BuildModeArchive BuildMode = "c-archive"
	// BuildModeShared produces a shared library (.so) and header (.h).
	BuildModeShared BuildMode = "c-shared"
)

// Option configures the Compiler.
type Option func(*Compiler)

// WithBuildMode sets the build mode used when invoking the Go tool.
func WithBuildMode(m BuildMode) Option { return func(c *Compiler) { c.mode = m } }

// Compiler translates a Mochi AST into C-compatible library files by
// first compiling the program to Go source and then using the Go toolchain
// to build a c-archive or c-shared library.
type Compiler struct {
	env  *types.Env
	mode BuildMode
}

// New creates a new C compiler instance.
func New(env *types.Env, opts ...Option) *Compiler {
	c := &Compiler{env: env, mode: BuildModeArchive}
	for _, opt := range opts {
		opt(c)
	}
	return c
}

// Compile returns a zip archive containing the generated C header and library
// implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	dir, err := os.MkdirTemp("", "mochi-c-")
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

	outBase := filepath.Join(dir, "prog")
	cmd := exec.Command("go", "build", "-buildmode="+string(c.mode), "-o", outBase, srcPath)
	cmd.Env = append(os.Environ(), "GO111MODULE=on")
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("go build failed: %w\n%s", err, out)
	}

	libData, err := os.ReadFile(outBase + ".a")
	if err != nil {
		return nil, err
	}
	hdrData, err := os.ReadFile(outBase + ".h")
	if err != nil {
		return nil, err
	}

	var buf bytes.Buffer
	zw := zip.NewWriter(&buf)
	fw, _ := zw.Create("prog.h")
	fw.Write(hdrData)
	fw, _ = zw.Create("prog.a")
	fw.Write(libData)
	zw.Close()

	return buf.Bytes(), nil
}
