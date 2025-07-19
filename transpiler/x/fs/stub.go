//go:build !slow

package fstrans

import (
	"mochi/parser"
	"mochi/types"
)

// Package fstrans provides an F# transpiler. This stub allows the package to
// compile when the 'slow' build tag is not enabled.

// Program is a minimal placeholder used when the real implementation is
// excluded by build tags.
type Program struct{}

// Transpile returns a nil program and no error. It exists so packages depending
// on fstrans compile without requiring the slow implementation.
func Transpile(_ *parser.Program, _ *types.Env) (*Program, error) {
	return &Program{}, nil
}

// Emit returns an empty byte slice. It is a no-op replacement for the real
// function provided when the "slow" build tag is enabled.
func Emit(_ *Program) []byte { return nil }
