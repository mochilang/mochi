//go:build !slow

package zigt

import (
	"mochi/parser"
	"mochi/types"
)

// Program is a placeholder used when the real implementation is excluded by build tags.
type Program struct{}

// SetBenchMain is a no-op in the stub build.
func SetBenchMain(v bool) { benchMain = v }

// Transpile returns a placeholder program.
func Transpile(prog *parser.Program, env *types.Env, bench bool) (*Program, error) {
	_ = bench
	return &Program{}, nil
}

// Emit returns an empty byte slice.
func (p *Program) Emit() []byte { return nil }
