//go:build slow

package ctrans

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// --- Simple C AST ---

type Program struct {
	Functions []*Function
}

type Function struct {
	Name string
	Body []Stmt
}

type Stmt interface {
	emit(io.Writer)
}

type CallStmt struct {
	Func string
	Args []Expr
}

type ReturnStmt struct {
	Expr Expr
}

func (c *CallStmt) emit(w io.Writer) {
	if c.Func == "print" && len(c.Args) == 1 {
		if s, ok := c.Args[0].(*StringLit); ok {
			fmt.Fprintf(w, "\tprintf(\"%s\\n\");\n", escape(s.Value))
		}
		return
	}
}

func (r *ReturnStmt) emit(w io.Writer) {
	io.WriteString(w, "\treturn 0;\n")
}

type Expr interface{}

type StringLit struct{ Value string }

func escape(s string) string {
	s = strings.ReplaceAll(s, "\\", "\\\\")
	s = strings.ReplaceAll(s, "\"", "\\\"")
	return s
}

// Emit generates C source from AST.
func (p *Program) Emit() []byte {
	var buf bytes.Buffer
	buf.WriteString("#include <stdio.h>\n\n")
	for _, f := range p.Functions {
		buf.WriteString("int ")
		buf.WriteString(f.Name)
		buf.WriteString("() {\n")
		for _, s := range f.Body {
			s.emit(&buf)
		}
		buf.WriteString("\treturn 0;\n")
		buf.WriteString("}\n")
	}
	return buf.Bytes()
}

// Transpile converts a Mochi program into a C AST.
func Transpile(env *types.Env, prog *parser.Program) (*Program, error) {
	mainFn := &Function{Name: "main"}
	for _, s := range prog.Statements {
		if s.Expr != nil {
			call := s.Expr.Expr.Binary.Left.Value.Target.Call
			if call != nil && call.Func == "print" && len(call.Args) == 1 {
				arg := call.Args[0]
				lit := arg.Binary.Left.Value.Target.Lit
				if lit != nil && lit.Str != nil {
					mainFn.Body = append(mainFn.Body, &CallStmt{Func: "print", Args: []Expr{&StringLit{Value: *lit.Str}}})
					continue
				}
			}
		}
	}
	p := &Program{Functions: []*Function{mainFn}}
	return p, nil
}

// CompileFile parses and type-checks a Mochi source file and transpiles it to C code.
