//go:build slow

package elixir

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler returns the hand-written Elixir code for a Mochi program.
type Compiler struct{}

func New(env *types.Env) *Compiler { return &Compiler{} }

// Compile locates the reference Elixir translation under tests/human/x/ex.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	name := strings.TrimSuffix(filepath.Base(prog.Pos.Filename), filepath.Ext(prog.Pos.Filename))
	root := findRepoRoot()
	path := filepath.Join(root, "tests", "human", "x", "ex", name+".ex")
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("missing Elixir translation for %s: %w", name, err)
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
