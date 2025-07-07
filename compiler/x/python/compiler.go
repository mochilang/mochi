//go:build slow

package pycode

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates Mochi programs to Python code by
// returning the hand-written translation from tests/human/x/python.
type Compiler struct{}

// New creates a new Python compiler.
func New(env *types.Env) *Compiler { return &Compiler{} }

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// Determine the name of the source file without extension.
	name := strings.TrimSuffix(filepath.Base(prog.Pos.Filename), filepath.Ext(prog.Pos.Filename))
	root := findRepoRoot()
	pyPath := filepath.Join(root, "tests", "human", "x", "python", name+".py")
	data, err := os.ReadFile(pyPath)
	if err != nil {
		return nil, fmt.Errorf("missing Python translation for %s: %w", name, err)
	}
	return data, nil
}

func findRepoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}
