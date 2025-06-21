package luacode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Lua source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env

	loopLabels []string
	labelCount int

	tmpCount int

	helpers  map[string]bool
	packages map[string]bool
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), packages: make(map[string]bool), tmpCount: 0}
}

// Compile returns Lua source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// reset state so the compiler can be reused
	c.buf.Reset()
	c.indent = 0
	c.loopLabels = nil
	c.labelCount = 0
	c.tmpCount = 0
	c.helpers = make(map[string]bool)
	c.packages = make(map[string]bool)

	// Emit function declarations first.
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun, false); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Emit test block declarations.
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Emit main body.
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}

	// Invoke test blocks.
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(name + "()")
		}
	}

	bodyBytes := append([]byte(nil), c.buf.Bytes()...)
	c.buf.Reset()
	c.emitHelpers()
	c.buf.Write(bodyBytes)

	code := c.buf.Bytes()
	if err := checkLuaSyntax(code); err != nil {
		return nil, err
	}

	return code, nil
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func checkLuaSyntax(code []byte) error {
	if _, err := exec.LookPath("luac"); err != nil {
		return nil
	}
	tmp, err := os.CreateTemp("", "mochi_*.lua")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.Write(code); err != nil {
		tmp.Close()
		return err
	}
	tmp.Close()
	cmd := exec.Command("luac", "-p", tmp.Name())
	if out, err := cmd.CombinedOutput(); err != nil {
		return fmt.Errorf("luac error: %v\n%s", err, out)
	}
	return nil
}
