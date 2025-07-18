//go:build slow

package csharpcode

import (
	cscode "mochi/compiler/x/cs"
	"mochi/parser"
	"mochi/types"
)

// Compiler wraps the existing cs compiler to expose it under the csharp package.
type Compiler struct {
	inner *cscode.Compiler
}

// New returns a new C# compiler.
func New(env *types.Env) *Compiler {
	return &Compiler{inner: cscode.New(env)}
}

// Compile compiles the Mochi program to C# source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	return c.inner.Compile(prog)
}
