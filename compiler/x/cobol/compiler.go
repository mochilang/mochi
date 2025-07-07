//go:build slow

package cobol

import (
	"bytes"
	"fmt"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf    bytes.Buffer
	indent int
	vars   []varDecl
	init   []string
	env    *types.Env
}

type varDecl struct {
	name string
	pic  string
	val  string
}

func (c *Compiler) hasVar(name string) bool {
	for _, v := range c.vars {
		if v.name == name {
			return true
		}
	}
	return false
}

func (c *Compiler) collectForVars(st []*parser.Statement) {
	for _, s := range st {
		switch {
		case s.For != nil:
			if !c.hasVar(s.For.Name) {
				c.vars = append(c.vars, varDecl{name: s.For.Name, pic: "PIC 9", val: "0"})
			}
			c.collectForVars(s.For.Body)
		case s.If != nil:
			c.collectForVars(s.If.Then)
			if s.If.ElseIf != nil {
				c.collectForVars([]*parser.Statement{{If: s.If.ElseIf}})
			}
			c.collectForVars(s.If.Else)
		case s.While != nil:
			c.collectForVars(s.While.Body)
		}
	}
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 7
	c.vars = nil
	c.init = nil

	name := strings.TrimSuffix(filepath.Base(prog.Pos.Filename), filepath.Ext(prog.Pos.Filename))
	name = strings.ReplaceAll(name, "-", "_")
	name = strings.ToUpper(name)

	// collect variable and constant declarations
	for _, st := range prog.Statements {
		switch {
		case st.Var != nil:
			if err := c.addVar(st.Var.Name, st.Var.Type, st.Var.Value, st.Var.Pos.Line); err != nil {
				return nil, err
			}
		case st.Let != nil:
			if err := c.addVar(st.Let.Name, st.Let.Type, st.Let.Value, st.Let.Pos.Line); err != nil {
				return nil, err
			}
		}
	}
	c.collectForVars(prog.Statements)

	c.writeln("IDENTIFICATION DIVISION.")
	c.writeln(fmt.Sprintf("PROGRAM-ID. %s.", name))
	c.writeln("DATA DIVISION.")
	c.writeln("WORKING-STORAGE SECTION.")
	for _, v := range c.vars {
		line := fmt.Sprintf("01 %s %s", strings.ToUpper(v.name), v.pic)
		if v.val != "" {
			line += " VALUE " + v.val + "."
		} else {
			line += "."
		}
		c.writeln(line)
	}
	c.writeln("PROCEDURE DIVISION.")
	for _, line := range c.init {
		c.writeln(line)
	}
	for _, st := range prog.Statements {
		if st.Var != nil || st.Let != nil {
			// already handled in declarations
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	c.writeln("STOP RUN.")
	return c.buf.Bytes(), nil
}

func (c *Compiler) addVar(name string, typ *parser.TypeRef, value *parser.Expr, line int) error {
	pic := "PIC 9"
	val := ""
	if typ != nil && typ.Simple != nil {
		switch *typ.Simple {
		case "string":
			pic = "PIC X"
		case "bool":
			pic = "PIC X(5)"
		default:
			pic = "PIC 9"
		}
	}
	if value != nil && isLiteralExpr(value) {
		v, err := c.compileExpr(value)
		if err != nil {
			return err
		}
		val = v
	} else if value == nil {
		switch {
		case typ != nil && typ.Simple != nil && *typ.Simple == "string":
			val = "\"\""
		case typ != nil && typ.Simple != nil && *typ.Simple == "bool":
			val = "FALSE"
		default:
			val = "0"
		}
	} else if value != nil {
		expr, err := c.compileExpr(value)
		if err != nil {
			return err
		}
		c.init = append(c.init, fmt.Sprintf("COMPUTE %s = %s", strings.ToUpper(name), expr))
	}
	if strings.HasPrefix(val, "\"") && strings.HasSuffix(val, "\"") {
		pic = fmt.Sprintf("PIC X(%d)", len(strings.Trim(val, "\"")))
	} else if strings.HasPrefix(val, "-") {
		pic = "PIC S9"
	}
	c.vars = append(c.vars, varDecl{name: name, pic: pic, val: val})
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	name := strings.ToUpper(a.Name)
	c.writeln(fmt.Sprintf("COMPUTE %s = %s", name, val))
	return nil
}

func isLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 1 {
		return false
	}
	if len(u.Ops) == 1 && u.Ops[0] != "-" {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	return p.Target != nil && p.Target.Lit != nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("IF " + cond)
	c.indent += 4
	for _, st := range i.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= 4
	if i.ElseIf != nil || len(i.Else) > 0 {
		c.writeln("ELSE")
		c.indent += 4
		if i.ElseIf != nil {
			if err := c.compileIf(i.ElseIf); err != nil {
				return err
			}
		} else {
			for _, st := range i.Else {
				if err := c.compileStmt(st); err != nil {
					return err
				}
			}
		}
		c.indent -= 4
	}
	c.writeln("END-IF")
	return nil
}

func negateOp(op string) (string, bool) {
	switch op {
	case "<":
		return ">=", true
	case "<=":
		return ">", true
	case ">":
		return "<=", true
	case ">=":
		return "<", true
	default:
		return "", false
	}
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	// attempt to convert while cond to PERFORM UNTIL not cond
	neg := "NOT (" + cond + ")"
	if b := w.Cond.Binary; b != nil && len(b.Right) == 1 {
		op := b.Right[0].Op
		if n, ok := negateOp(op); ok {
			left, _ := c.compileUnary(b.Left)
			right, _ := c.compilePostfix(b.Right[0].Right)
			neg = fmt.Sprintf("%s %s %s", left, n, right)
		}
	}
	c.writeln("PERFORM UNTIL " + neg)
	c.indent += 4
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= 4
	c.writeln("END-PERFORM")
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if f.RangeEnd == nil {
		return fmt.Errorf("only range for loops supported")
	}
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(f.RangeEnd)
	if err != nil {
		return err
	}
	name := strings.ToUpper(f.Name)
	c.writeln(fmt.Sprintf("PERFORM VARYING %s FROM %s BY 1 UNTIL %s > %s", name, start, name, end))
	c.indent += 4
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent -= 4
	c.writeln("END-PERFORM")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
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
		res = fmt.Sprintf("%s %s %s", res, op.Op, r)
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = u.Ops[i] + " " + val
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
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Selector != nil:
		return strings.ToUpper(p.Selector.Root), nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	if call.Func != "print" || len(call.Args) != 1 {
		return "", fmt.Errorf("unsupported call %s", call.Func)
	}
	arg, err := c.compileExpr(call.Args[0])
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("DISPLAY %s", arg), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if *l.Bool {
			return "TRUE"
		}
		return "FALSE"
	default:
		return ""
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte(' ')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
