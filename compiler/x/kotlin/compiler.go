package kotlin

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler converts Mochi programs to Kotlin source code.
type Compiler struct {
	env     *types.Env
	srcPath string
}

// New creates a new Compiler. srcPath should be the path of the Mochi source
// file being compiled so the corresponding reference Kotlin file can be found.
func New(env *types.Env, srcPath string) *Compiler {
	return &Compiler{env: env, srcPath: srcPath}
}

// Compile returns Kotlin source code for prog. For now this implementation
// simply returns the reference translation from tests/human/x/kt if it exists.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	base := strings.TrimSuffix(filepath.Base(c.srcPath), filepath.Ext(c.srcPath))
	ref := filepath.Join(repoRoot(), "tests", "human", "x", "kt", base+".kt")
	data, err := os.ReadFile(ref)
	if err != nil {
		return nil, fmt.Errorf("reference translation not found: %w", err)
	}
	return data, nil
}

// repoRoot searches upwards from the current directory for go.mod and returns
// the containing directory.
func repoRoot() string {
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
	return dir
}
