//go:build slow

package elxir

import (
	excode "mochi/compiler/x/ex"
	"mochi/parser"
	"mochi/types"
)

// Compiler wraps the existing ex compiler to produce Elixir code.
type Compiler struct {
	inner *excode.Compiler
}

func New(env *types.Env) *Compiler {
	return &Compiler{inner: excode.New(env)}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	return c.inner.Compile(prog)
}

func Ensure() error { return excode.Ensure() }

func EnsureElixir() error { return excode.EnsureElixir() }

func Format(code []byte) ([]byte, error) { return excode.Format(code) }
