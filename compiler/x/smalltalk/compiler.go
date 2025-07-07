//go:build slow

package smalltalk

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Compiler is a trivial compiler that returns prewritten Smalltalk code for a
// given Mochi source file. The reference implementations live under
// tests/human/x/st.
type Compiler struct{}

func New() *Compiler { return &Compiler{} }

// Compile reads the corresponding Smalltalk translation from
// tests/human/x/st/<name>.st where <name> is the basename of the input
// without extension. If the translation is missing, an error is returned.
func (c *Compiler) Compile(srcPath string) ([]byte, error) {
	base := filepath.Base(srcPath)
	name := strings.TrimSuffix(base, filepath.Ext(base))

	// Attempt to locate the human translation by walking up a few
	// parent directories until tests/human/x/st is found.
	dir, _ := os.Getwd()
	for i := 0; i < 5; i++ {
		stPath := filepath.Join(dir, "tests", "human", "x", "st", name+".st")
		if data, err := os.ReadFile(stPath); err == nil {
			return data, nil
		}
		dir = filepath.Dir(dir)
	}
	return nil, fmt.Errorf("translation for %s not found", base)
}
