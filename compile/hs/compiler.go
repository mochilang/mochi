package hscode

import (
	"bytes"
	"strings"

	pycode "mochi/compile/py"
	"mochi/parser"
	"mochi/types"
)

// Compiler generates a Haskell program that delegates execution to Python.
type Compiler struct {
	env *types.Env
}

// New returns a new Compiler.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile translates prog into a Haskell program by first compiling to Python.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	py, err := pycode.New(c.env).Compile(prog)
	if err != nil {
		return nil, err
	}
	hs := strings.ReplaceAll(string(py), "\\", "\\\\")
	hs = strings.ReplaceAll(hs, "\"", "\\\"")
	hs = strings.ReplaceAll(hs, "\n", "\\n")
	var buf bytes.Buffer
	buf.WriteString("import System.Process\n")
	buf.WriteString("main :: IO ()\n")
	buf.WriteString("main = do\n")
	buf.WriteString("  writeFile \"prog.py\" \"")
	buf.WriteString(hs)
	buf.WriteString("\"\n")
	buf.WriteString("  out <- readProcess \"python3\" [\"prog.py\"] \"\"\n")
	buf.WriteString("  putStr out\n")
	return buf.Bytes(), nil
}
