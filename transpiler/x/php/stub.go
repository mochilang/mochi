//go:build !slow

package php

import (
	"io"

	"mochi/parser"
	"mochi/types"
)

// Program is a minimal placeholder used when the real implementation is excluded by build tags.
type Program struct{}

// Transpile returns a placeholder program so dependent packages compile without the slow implementation.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	return &Program{}, nil
}

// Emit is a no-op replacement for the real function.
func Emit(w io.Writer, p *Program) error { return nil }
