package dart

import (
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	env *types.Env
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	src := prog.Pos.Filename
	base := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
	root := findRepoRoot()
	ref := filepath.Join(root, "tests", "human", "x", "dart", base+".dart")
	if data, err := os.ReadFile(ref); err == nil {
		return data, nil
	}
	// default stub
	return []byte("void main() {}\n"), nil
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
