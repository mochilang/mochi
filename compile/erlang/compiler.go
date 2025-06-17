package erlcode

import (
	"bytes"
	"strings"

	pycode "mochi/compile/py"
	"mochi/parser"
	"mochi/types"
)

// Compiler generates an Erlang escript that delegates execution to Python.
type Compiler struct {
	env *types.Env
}

// New returns a new Compiler.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile translates prog into an Erlang escript by first compiling to Python.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	py, err := pycode.New(c.env).Compile(prog)
	if err != nil {
		return nil, err
	}
	esc := strings.ReplaceAll(string(py), "\n", "\\n")
	esc = strings.ReplaceAll(esc, "\"", "\\\"")
	var buf bytes.Buffer
	buf.WriteString("#!/usr/bin/env escript\n")
	buf.WriteString("-module(main).\n")
	buf.WriteString("-export([main/1]).\n")
	buf.WriteString("main(_) ->\n")
	buf.WriteString("  Code = \"")
	buf.WriteString(esc)
	buf.WriteString("\",\n")
	buf.WriteString("  file:write_file('prog.py', Code),\n")
	buf.WriteString("  Out = os:cmd(\"python3 prog.py\"),\n")
	buf.WriteString("  io:format(\"~s\", [Out]).\n")
	return buf.Bytes(), nil
}
