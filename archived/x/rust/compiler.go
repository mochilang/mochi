//go:build archived

package rscode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Rust source code.
type Compiler struct {
	buf           bytes.Buffer
	indent        int
	env           *types.Env
	helpers       map[string]bool
	structs       map[string]bool
	retType       string
	methodFields  map[string]bool
	externs       []string
	externObjects []string
	locals        map[string]types.Type
}

// New creates a new Rust compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), structs: make(map[string]bool), methodFields: nil, externs: []string{}, externObjects: []string{}, locals: map[string]types.Type{}}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile returns Rust source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// gather extern declarations
	for _, stmt := range prog.Statements {
		switch {
		case stmt.ExternVar != nil:
			name := sanitizeName(stmt.ExternVar.Name())
			typ := rustType(stmt.ExternVar.Type)
			c.externs = append(c.externs, fmt.Sprintf("    pub static mut %s: %s;", name, typ))
			if c.env != nil {
				c.env.SetVar(stmt.ExternVar.Name(), c.resolveTypeRef(stmt.ExternVar.Type), true)
			}
		case stmt.ExternFun != nil:
			params := make([]string, len(stmt.ExternFun.Params))
			var ptypes []types.Type
			for i, p := range stmt.ExternFun.Params {
				params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), rustType(p.Type))
				if c.env != nil {
					ptypes = append(ptypes, c.resolveTypeRef(p.Type))
				}
			}
			ret := "()"
			if stmt.ExternFun.Return != nil {
				ret = rustType(stmt.ExternFun.Return)
			}
			sig := fmt.Sprintf("    pub fn %s(%s) -> %s;", sanitizeName(stmt.ExternFun.Name()), strings.Join(params, ", "), ret)
			c.externs = append(c.externs, sig)
			if c.env != nil {
				ft := types.FuncType{Params: ptypes, Return: c.resolveTypeRef(stmt.ExternFun.Return)}
				c.env.SetVar(stmt.ExternFun.Name(), ft, true)
			}
		case stmt.ExternType != nil:
			name := sanitizeName(stmt.ExternType.Name)
			c.externs = append(c.externs, fmt.Sprintf("    pub type %s;", name))
			if c.env != nil {
				st := types.StructType{Name: stmt.ExternType.Name, Fields: map[string]types.Type{}, Order: []string{}, Methods: map[string]types.Method{}}
				c.env.SetStruct(stmt.ExternType.Name, st)
			}
		case stmt.ExternObject != nil:
			c.externObjects = append(c.externObjects, sanitizeName(stmt.ExternObject.Name))
			if c.env != nil {
				c.env.SetVar(stmt.ExternObject.Name, types.AnyType{}, true)
			}
		}
	}

	for _, stmt := range prog.Statements {
		if stmt.Type != nil {
			if err := c.compileTypeDecl(stmt.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, stmt := range prog.Statements {
		if stmt.Fun != nil {
			if err := c.compileFun(stmt.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, stmt := range prog.Statements {
		if stmt.Test != nil {
			if err := c.compileTestBlock(stmt.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	c.writeln("fn main() {")
	c.indent++
	for _, stmt := range prog.Statements {
		if stmt.Fun == nil && stmt.Test == nil {
			if err := c.compileStmt(stmt); err != nil {
				return nil, err
			}
		}
	}
	for _, stmt := range prog.Statements {
		if stmt.Test != nil {
			name := "test_" + sanitizeName(stmt.Test.Name)
			c.writeln(fmt.Sprintf("%s();", name))
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	if len(c.externs) > 0 {
		c.writeln("extern \"C\" {")
		c.indent++
		for _, ex := range c.externs {
			c.writeln(ex)
		}
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	c.emitRuntime()
	return FormatRust(c.buf.Bytes()), nil
}
