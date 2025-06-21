package plcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	tmp     int
	vars    map[string]string
	currFun string
	helpers map[string]bool
	tests   []string
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, vars: make(map[string]string), helpers: make(map[string]bool), tests: []string{}}
}

// Compile translates a Mochi AST into Prolog source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()

	var body bytes.Buffer
	origBuf := c.buf
	c.buf = body

	// function declarations, facts, rules and tests
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Fact != nil {
			if err := c.compileFact(s.Fact); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Rule != nil {
			if err := c.compileRule(s.Rule); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	tmpBuf := c.buf
	tmpIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = tmpIndent + 1
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil || s.Fact != nil || s.Rule != nil {
			continue
		}
		if err := c.compileStmt(s, "_"); err != nil {
			c.buf = origBuf
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeAtom(strings.ReplaceAll(s.Test.Name, " ", "_"))
			c.writeln(fmt.Sprintf("%s,", name))
		}
	}
	b := c.buf.Bytes()
	if bytes.HasSuffix(b, []byte(",\n")) {
		b = b[:len(b)-2]
		b = append(b, '\n')
	}
	c.buf = tmpBuf
	if len(b) == 0 {
		c.writeln("main :- true.")
	} else {
		c.writeln("main :-")
		c.buf.Write(b)
		c.writeln(".")
	}
	c.indent = tmpIndent
	c.writeln(":- initialization(main, main).")

	bodyBytes := append([]byte(nil), c.buf.Bytes()...)
	c.buf = origBuf
	c.indent = 0
	c.writeln(":- style_check(-singleton).")
	c.emitHelpers()
	c.buf.Write(bodyBytes)

	return c.buf.Bytes(), nil
}

func (c *Compiler) newVar() string {
	v := fmt.Sprintf("_V%d", c.tmp)
	c.tmp++
	return v
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitHelpers() {
	if c.helpers["slice"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperSlice, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["tolist"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperToList, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["getitem"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperGetItem, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["setitem"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperSetItem, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["contains"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperContains, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["input"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperInput, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["count"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperCount, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["avg"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperAvg, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["expect"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperExpect, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
}
