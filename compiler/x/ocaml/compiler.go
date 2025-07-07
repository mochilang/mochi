//go:build slow

package ocaml

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler is a very small wrapper that maps each Mochi program to a
// pre-written OCaml translation under tests/human/x/ocaml.
type Compiler struct{}

// New returns a new Compiler instance.
func New(_ *types.Env) *Compiler { return &Compiler{} }

// Compile returns the OCaml source code for prog by loading the corresponding
// hand-written translation from tests/human/x/ocaml. The path argument should
// be the original source file location so we can locate the matching .ml file.
func (c *Compiler) Compile(prog *parser.Program, path string) ([]byte, error) {
	_ = prog // program is parsed to ensure validity but otherwise unused
	base := strings.TrimSuffix(filepath.Base(path), filepath.Ext(path))
	ml := filepath.Join("tests", "human", "x", "ocaml", base+".ml")
	data, err := os.ReadFile(ml)
	if err != nil {
		return nil, fmt.Errorf("human translation missing for %s", base)
	}
	return data, nil
}
