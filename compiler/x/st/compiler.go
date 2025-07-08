package st

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

// Compiler translates a subset of Mochi to Smalltalk source code.
type Compiler struct {
	buf  bytes.Buffer
	vars map[string]bool
}

// New returns a new compiler instance.
func New() *Compiler {
	return &Compiler{vars: make(map[string]bool)}
}

// Compile converts a parsed Mochi program to Smalltalk code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.vars = make(map[string]bool)
	for _, st := range p.Statements {
		c.collectVars(st)
	}
	if len(c.vars) > 0 {
		c.buf.WriteString("| ")
		names := make([]string, 0, len(c.vars))
		for n := range c.vars {
			names = append(names, n)
		}
		c.buf.WriteString(strings.Join(names, " "))
		c.buf.WriteString(" |\n")
	}
	for _, st := range p.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) collectVars(st *parser.Statement) {
	switch {
	case st.Let != nil:
		c.vars[st.Let.Name] = true
	case st.Var != nil:
		c.vars[st.Var.Name] = true
	case st.If != nil:
		for _, s := range st.If.Then {
			c.collectVars(s)
		}
		if st.If.ElseIf != nil {
			c.collectVars(&parser.Statement{If: st.If.ElseIf})
		}
		for _, s := range st.If.Else {
			c.collectVars(s)
		}
	case st.While != nil:
		for _, s := range st.While.Body {
			c.collectVars(s)
		}
	case st.For != nil:
		c.vars[st.For.Name] = true
		for _, s := range st.For.Body {
			c.collectVars(s)
		}
	}
}

func (c *Compiler) writeln(s string) {
	c.buf.WriteString(s)
	if !strings.HasSuffix(s, ".") {
		c.buf.WriteByte('.')
	}
	c.buf.WriteByte('\n')
}

func (c *Compiler) compileStmt(st *parser.Statement) error {
	switch {
	case st.Let != nil:
		expr, err := c.compileExpr(st.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := %s", st.Let.Name, expr))
	case st.Var != nil:
		expr, err := c.compileExpr(st.Var.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := %s", st.Var.Name, expr))
	case st.Assign != nil:
		if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
			return fmt.Errorf("complex assignment not supported")
		}
		val, err := c.compileExpr(st.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := %s", st.Assign.Name, val))
	case st.Expr != nil:
		// handle print built-in
		if call := getCall(st.Expr.Expr); call != nil && call.Func == "print" && len(call.Args) == 1 {
			arg, err := c.compileExpr(call.Args[0])
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("Transcript show: (%s) printString; cr", arg))
			return nil
		}
		expr, err := c.compileExpr(st.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case st.If != nil:
		return c.compileIf(st.If)
	case st.While != nil:
		return c.compileWhile(st.While)
	case st.For != nil:
		return c.compileFor(st.For)
	default:
		return fmt.Errorf("unsupported statement at line %d", st.Pos.Line)
	}
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.buf.WriteString("(" + cond + ") ifTrue: [\n")
	for _, st := range i.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.buf.WriteString("]")
	if len(i.Else) > 0 {
		c.buf.WriteString(" ifFalse: [\n")
		for _, st := range i.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.buf.WriteString("]")
	}
	c.writeln("")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.buf.WriteString("[(" + cond + ")] whileTrue: [\n")
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("]")
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	src, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	if f.RangeEnd != nil {
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.buf.WriteString(fmt.Sprintf("%s to: %s do: [:%s |\n", src, end, f.Name))
	} else {
		c.buf.WriteString(fmt.Sprintf("%s do: [:%s |\n", src, f.Name))
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("]")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expression")
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
		if u.Ops[i] == "-" {
			val = "-" + val
		} else if u.Ops[i] == "!" {
			val = "not " + val
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
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "{" + strings.Join(elems, ". ") + "}", nil
	case p.Map != nil:
		pairs := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			pairs[i] = fmt.Sprintf("%s -> %s", k, v)
		}
		return "Dictionary newFrom: {" + strings.Join(pairs, ". ") + "}", nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		if len(args) == 0 {
			return p.Call.Func, nil
		}
		return fmt.Sprintf("%s(%s)", p.Call.Func, strings.Join(args, ", ")), nil
	case p.Selector != nil:
		parts := append([]string{p.Selector.Root}, p.Selector.Tail...)
		return strings.Join(parts, "."), nil
	case p.Group != nil:
		s, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + s + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
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
		return fmt.Sprintf("'%s'", strings.Trim(*l.Str, "\""))
	case l.Null:
		return "nil"
	default:
		return ""
	}
}

func getCall(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil {
		return nil
	}
	b := e.Binary
	if len(b.Right) == 0 && len(b.Left.Ops) == 0 && b.Left.Value != nil {
		if c := b.Left.Value.Target.Call; c != nil {
			return c
		}
	}
	return nil
}
