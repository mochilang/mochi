//go:build !slow

package cstranspiler

import (
	"mochi/parser"
	"mochi/types"
)

// Program is a minimal placeholder used when the real implementation is
// excluded by build tags.
type Program struct{}

// Transpile returns a placeholder program and no error so that packages can
// compile without the slow implementation.
func Transpile(_ *parser.Program, _ *types.Env) (*Program, error) {
	return &Program{}, nil
}

// Emit returns nil output. It is a no-op replacement when the real
// implementation is not included.
func Emit(_ *Program) []byte { return nil }
