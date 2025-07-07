//go:build archived

package mlir

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	ccode "mochi/archived/x/c"
	"mochi/parser"
)

// Compiler translates Mochi programs to MLIR by first compiling
// them to C and then importing the generated LLVM IR.
type Compiler struct {
	clang string
}

// New creates a new MLIR compiler. The clang binary is located via PATH.
func New() (*Compiler, error) {
	if path, err := exec.LookPath("clang-19"); err == nil {
		return &Compiler{clang: path}, nil
	}
	if path, err := exec.LookPath("clang"); err == nil {
		return &Compiler{clang: path}, nil
	}
	return nil, fmt.Errorf("clang compiler not found")
}

// Compile generates MLIR for the given program.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// Generate C source using the existing C backend.
	src, err := ccode.New(nil).Compile(prog)
	if err != nil {
		return nil, err
	}

	tmpDir, err := os.MkdirTemp("", "mochi-mlir-")
	if err != nil {
		return nil, err
	}
	defer os.RemoveAll(tmpDir)

	cFile := filepath.Join(tmpDir, "main.c")
	if err := os.WriteFile(cFile, src, 0644); err != nil {
		return nil, err
	}
	llFile := filepath.Join(tmpDir, "main.ll")
	cmd := exec.Command(c.clang, "-S", "-emit-llvm", cFile, "-o", llFile)
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("clang failed: %v\n%s", err, out)
	}
	// Import the LLVM IR into MLIR.
	cmd = exec.Command("mlir-translate-19", "--import-llvm", llFile)
	data, err := cmd.Output()
	if err != nil {
		if ee, ok := err.(*exec.ExitError); ok {
			return nil, fmt.Errorf("mlir-translate failed: %v\n%s", err, string(ee.Stderr))
		}
		return nil, err
	}
	return data, nil
}
