package rubycode

import (
	rbcode "mochi/compiler/x/rb"
	"mochi/parser"
	"mochi/types"
)

// Compiler is an alias of rbcode.Compiler for Ruby output.
type Compiler struct {
	c *rbcode.Compiler
}

func New(env *types.Env) *Compiler {
	return &Compiler{c: rbcode.New(env)}
}

// Compile delegates to the rb compiler.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	return c.c.Compile(prog)
}
