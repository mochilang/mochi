//go:build slow

package ohp

import (
	"io"

	"mochi/parser"
	php "mochi/transpiler/x/php"
	"mochi/types"
)

// SetBenchMain proxies to the PHP transpiler's benchmark configuration.
func SetBenchMain(v bool) { php.SetBenchMain(v) }

// Program is the PHP transpiler's program representation.
type Program = php.Program

// Transpile forwards to the PHP transpiler.
func Transpile(p *parser.Program, env *types.Env) (*php.Program, error) {
	return php.Transpile(p, env)
}

// Emit writes out PHP code using the underlying PHP transpiler.
func Emit(w io.Writer, prog *php.Program) error {
	return php.Emit(w, prog)
}
