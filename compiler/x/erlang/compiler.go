//go:build slow

package erlang

import (
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
)

// Compiler translates a subset of Mochi programs into Erlang source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	srcPath string

	varVers map[string]int
	lets    map[string]bool
}

func New(srcPath string) *Compiler {
	return &Compiler{srcPath: srcPath, varVers: make(map[string]int), lets: make(map[string]bool)}
}

// CompileFile compiles the Mochi source file at path.
func CompileFile(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, err
	}
	c := New(path)
	return c.Compile(prog)
}

// WriteFile compiles srcPath and writes Erlang code to dstPath.
func WriteFile(srcPath, dstPath string) error {
	code, err := CompileFile(srcPath)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(dstPath), 0o755); err != nil {
		return err
	}
	return os.WriteFile(dstPath, code, 0o755)
}

func ListPrograms() ([]string, error) {
	return filepath.Glob(filepath.Join("tests", "vm", "valid", "*.mochi"))
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func RemoveOldArtifacts(dir string) error {
	return filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		os.Remove(path)
		return nil
	})
}

// Compile emits Erlang source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.varVers = make(map[string]int)
	c.lets = make(map[string]bool)

	base := strings.TrimSuffix(filepath.Base(c.srcPath), filepath.Ext(c.srcPath))
	c.writeln("#!/usr/bin/env escript")
	c.writeln(fmt.Sprintf("%% %s.erl - generated from %s", base, filepath.Base(c.srcPath)))
	c.writeln("")
	c.writeln("main(_) ->")
	c.indent++

	var lines []string
	for _, st := range prog.Statements {
		switch {
		case st.Let != nil:
			l, err := c.compileLet(st.Let)
			if err != nil {
				return nil, err
			}
			lines = append(lines, l)
		case st.Var != nil:
			l, err := c.compileVar(st.Var)
			if err != nil {
				return nil, err
			}
			lines = append(lines, l)
		case st.Assign != nil:
			l, err := c.compileAssign(st.Assign)
			if err != nil {
				return nil, err
			}
			lines = append(lines, l)
		case st.Expr != nil:
			e, err := c.compileExpr(st.Expr.Expr)
			if err != nil {
				return nil, err
			}
			lines = append(lines, e)
		default:
			return nil, fmt.Errorf("unsupported statement at line %d", st.Pos.Line)
		}
	}

	for i, l := range lines {
		if i == len(lines)-1 {
			c.writeln(l + ".")
		} else {
			c.writeln(l + ",")
		}
	}

	c.indent--
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileLet(l *parser.LetStmt) (string, error) {
	c.lets[l.Name] = true
	val := "undefined"
	if l.Value != nil {
		v, err := c.compileExpr(l.Value)
		if err != nil {
			return "", err
		}
		val = v
	}
	return fmt.Sprintf("%s = %s", capitalize(l.Name), val), nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) (string, error) {
	name := c.newVarName(v.Name)
	val := "undefined"
	if v.Value != nil {
		s, err := c.compileExpr(v.Value)
		if err != nil {
			return "", err
		}
		val = s
	}
	return fmt.Sprintf("%s = %s", name, val), nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) (string, error) {
	if len(a.Index) > 0 || len(a.Field) > 0 {
		return "", fmt.Errorf("complex assignment not supported")
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return "", err
	}
	name := c.newVarName(a.Name)
	return fmt.Sprintf("%s = %s", name, val), nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		opStr, err := c.mapBinOp(op.Op)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("(%s %s %s)", res, opStr, r)
	}
	return res, nil
}

func (c *Compiler) mapBinOp(op string) (string, error) {
	switch op {
	case "+", "-", "*", "/":
		return op, nil
	case "%":
		return "rem", nil
	case "==":
		return "==", nil
	case "!=":
		return "/=", nil
	case "<", "<=", ">", ">=":
		return op, nil
	case "&&":
		return "andalso", nil
	case "||":
		return "orelse", nil
	default:
		return "", fmt.Errorf("unsupported operator %s", op)
	}
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = "-" + val
		case "!":
			val = "not " + val
		default:
			return "", fmt.Errorf("unsupported unary op %s", u.Ops[i])
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	if len(p.Ops) > 0 {
		return "", fmt.Errorf("postfix operations not supported")
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Selector != nil:
		if len(p.Selector.Tail) > 0 {
			return "", fmt.Errorf("field access not supported")
		}
		return c.refVar(p.Selector.Root), nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	if call.Func == "print" {
		if len(call.Args) != 1 {
			return "", fmt.Errorf("print expects one argument")
		}
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("io:format(\"~p~n\", [%s])", arg), nil
	}
	return "", fmt.Errorf("unsupported function %s", call.Func)
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Null:
		return "undefined"
	default:
		return "undefined"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) newVarName(base string) string {
	idx := c.varVers[base]
	name := fmt.Sprintf("%s%d", capitalize(base), idx)
	c.varVers[base] = idx + 1
	return name
}

func (c *Compiler) refVar(base string) string {
	if c.lets[base] {
		return capitalize(base)
	}
	idx := c.varVers[base]
	if idx == 0 {
		return capitalize(base)
	}
	return fmt.Sprintf("%s%d", capitalize(base), idx-1)
}

func capitalize(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}
