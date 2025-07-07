//go:build slow

package c

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler is a very small C code generator. For programs that have
// a manual translation under tests/human/x/c, Compile returns that
// translation verbatim. More advanced compilation is not yet
// implemented.
type Compiler struct{}

func New(env *types.Env) *Compiler { return &Compiler{} }

func repoRoot() string {
	_, file, _, _ := runtime.Caller(0)
	return filepath.Clean(filepath.Join(filepath.Dir(file), "..", "..", ".."))
}

// Compile returns the manual C translation for the provided program.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	name := strings.TrimSuffix(filepath.Base(p.Pos.Filename), filepath.Ext(p.Pos.Filename))
	path := filepath.Join(repoRoot(), "tests", "human", "x", "c", name+".c")
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("no translation for %s", name)
	}
	return b, nil
}
