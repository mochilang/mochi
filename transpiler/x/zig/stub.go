//go:build !slow

package zigt

import (
	"mochi/parser"
	"mochi/types"
)

// benchMain tracks whether generated programs should wrap main in a benchmark
// block. It exists in the stub build so callers can set the value without
// referencing an undefined symbol when the real implementation is excluded by
// build tags.
var benchMain bool

// Program is a placeholder used when the real implementation is excluded by build tags.
type Program struct{}

// benchMain controls whether the main function is wrapped in a benchmark block.
var benchMain bool

// SetBenchMain is a no-op in the stub build.
func SetBenchMain(v bool) { benchMain = v }

// Transpile returns a placeholder program.
func Transpile(prog *parser.Program, env *types.Env, bench bool) (*Program, error) {
	_ = bench
	return &Program{}, nil
}

// Emit returns an empty byte slice.
func (p *Program) Emit() []byte { return nil }
