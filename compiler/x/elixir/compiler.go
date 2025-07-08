//go:build slow

package elixir

// Package elixir provides a thin wrapper around the existing excode package
// so that the Elixir compiler can be referenced using a more explicit name.
// It simply re-exports the excode Compiler implementation.

import (
	excode "mochi/compiler/x/ex"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates Mochi ASTs into Elixir source code. It embeds the
// excode.Compiler to reuse the full-featured implementation.
type Compiler struct{ *excode.Compiler }

// New returns a new Elixir compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{excode.New(env)} }

// Compile delegates to the underlying excode compiler.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) { return c.Compiler.Compile(p) }

// Ensure verifies that the Elixir toolchain is installed. Provided for
// backwards compatibility with other compiler packages.
func Ensure() error { return excode.Ensure() }

// Format runs the Elixir formatter on the given code.
func Format(b []byte) ([]byte, error) { return excode.Format(b) }
