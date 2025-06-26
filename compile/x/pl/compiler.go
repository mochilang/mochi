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
	lambdas []string
	funVars map[string]string
	tests   []string
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, vars: make(map[string]string), helpers: make(map[string]bool), tests: []string{}, lambdas: []string{}, funVars: make(map[string]string)}
}

// Compile translates a Mochi AST into Prolog source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()

	var body bytes.Buffer
	origBuf := c.buf
	c.buf = body

	// function declarations, type declarations, facts, rules and tests
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
		} else if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
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
	for _, name := range c.tests {
		c.writeln(fmt.Sprintf("%s,", name))
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
	for _, l := range c.lambdas {
		for _, line := range strings.Split(strings.TrimSuffix(l, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
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
	if c.helpers["sum"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperSum, "\n"), "\n") {
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
	if c.helpers["union_all"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperUnionAll, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["union"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperUnion, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["except"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperExcept, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["intersect"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperIntersect, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["map_keys"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperMapKeys, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["dataset_filter"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperDatasetFilter, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["dataset_paginate"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperDatasetPaginate, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["group_by"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperGroupBy, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["json"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperJSON, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["load_data"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperLoad, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["save_data"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperSave, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
}
