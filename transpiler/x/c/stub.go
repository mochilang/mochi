//go:build !slow

package ctrans

import (
	"mochi/parser"
	"mochi/types"
)

// Program is a minimal placeholder used when the real implementation is excluded by build tags.
type Program struct{}

var benchMain bool

// SetBenchMain is a no-op in the stub build.
func SetBenchMain(v bool) { benchMain = v }

// Transpile returns a placeholder program so dependent packages compile without the slow implementation.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	return &Program{}, nil
}

// Emit returns an empty byte slice. It is a no-op replacement for the real function.
func (p *Program) Emit() []byte { return nil }
