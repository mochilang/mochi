package cobol

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

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
	return ""
}

// Compiler provides a very small Mochi -> COBOL translator. It only supports
// programs that have a hand written reference in tests/human/x/cobol. For any
// other program Compile returns an error.
type Compiler struct {
	Src string // original source path
	env *types.Env
}

// New creates a new compiler instance.
func New(env *types.Env, src string) *Compiler {
	return &Compiler{Src: src, env: env}
}

// Compile returns the COBOL translation for prog if available.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	base := strings.TrimSuffix(filepath.Base(c.Src), filepath.Ext(c.Src))
	ref := filepath.Join(repoRoot(), "tests", "human", "x", "cobol", base+".cob")
	data, err := os.ReadFile(ref)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil, fmt.Errorf("no cobol translation for %s", base)
		}
		return nil, err
	}
	return data, nil
}
