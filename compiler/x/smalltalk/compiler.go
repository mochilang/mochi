//go:build slow

package smalltalk

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
)

// Compiler translates a limited subset of Mochi into GNU Smalltalk code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
}

// New returns a new Smalltalk compiler.
func New() *Compiler { return &Compiler{} }

// Compile converts the given Mochi program into Smalltalk source code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	vars := collectVars(p.Statements)
	if len(vars) > 0 {
		c.writeln("| " + strings.Join(vars, " ") + " |")
	}
	for _, st := range p.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func collectVars(st []*parser.Statement) []string {
	m := map[string]bool{}
	var walk func([]*parser.Statement)
	walk = func(ss []*parser.Statement) {
		for _, s := range ss {
			switch {
			case s.Let != nil:
				m[s.Let.Name] = true
			case s.Var != nil:
				m[s.Var.Name] = true
			case s.Fun != nil:
				m[s.Fun.Name] = true
				walk(s.Fun.Body)
			case s.If != nil:
				walk(s.If.Then)
				if s.If.Else != nil {
					walk(s.If.Else)
				}
				if s.If.ElseIf != nil {
					walk(s.If.ElseIf.Then)
					if s.If.ElseIf.Else != nil {
						walk(s.If.ElseIf.Else)
					}
				}
			case s.While != nil:
				walk(s.While.Body)
			case s.For != nil:
				walk(s.For.Body)
			}
		}
	}
	walk(st)
	names := make([]string, 0, len(m))
	for k := range m {
		names = append(names, k)
	}
	sort.Strings(names)
	return names
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ".")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	if l.Value == nil {
		return nil
	}
	val, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s.", l.Name, val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	if v.Value == nil {
		return nil
	}
	val, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s.", v.Name, val))
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	if len(a.Index) > 0 || len(a.Field) > 0 {
		return fmt.Errorf("complex assignment not supported")
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s.", a.Name, val))
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = ":" + p.Name
	}
	body, err := c.blockString(f.Body)
	if err != nil {
		return err
	}
	block := fmt.Sprintf("[%s | %s ]", strings.Join(params, " "), body)
	c.writeln(fmt.Sprintf("%s := %s.", f.Name, block))
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(%s) ifTrue: [", cond))
	c.indent++
	for _, st := range i.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.Else == nil {
		c.writeln("] .")
		return nil
	}
	c.writeln("] ifFalse: [")
	c.indent++
	for _, st := range i.Else {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("].")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("[%s] whileTrue: [", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("].")
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s to: %s do: [:%s |", start, end, f.Name))
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s do: [:%s |", src, f.Name))
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("].")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expression")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	// Convert the flat list of operations into a precedence aware expression
	// using the same precedence levels as the interpreter. This ensures
	// results match Mochi's semantics when translated to Smalltalk which has
	// different operator precedence rules.

	if b == nil {
		return "", fmt.Errorf("nil binary expression")
	}

	operands := []string{}
	ops := []string{}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, first)
	for _, p := range b.Right {
		o, err := c.compilePostfix(p.Right)
		if err != nil {
			return "", err
		}
		op := p.Op
		if p.All {
			op = op + "_all"
		}
		ops = append(ops, op)
		operands = append(operands, o)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	apply := func(left string, op string, right string) string {
		switch op {
		case "&&":
			return fmt.Sprintf("(%s and: [%s])", left, right)
		case "||":
			return fmt.Sprintf("(%s or: [%s])", left, right)
		case "==":
			return fmt.Sprintf("(%s = %s)", left, right)
		case "!=":
			return fmt.Sprintf("(%s ~= %s)", left, right)
		default:
			return fmt.Sprintf("(%s %s %s)", left, op, right)
		}
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range level {
				if ops[i] == t {
					res := apply(operands[i], ops[i], operands[i+1])
					operands[i] = res
					operands = append(operands[:i+1], operands[i+2:]...)
					ops = append(ops[:i], ops[i+1:]...)
					matched = true
					break
				}
			}
			if !matched {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("expression reduction failed")
	}
	return operands[0], nil
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
			val = val + " not"
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			if val == "print" {
				if len(args) == 0 {
					return "", fmt.Errorf("print expects at least 1 arg")
				}
				stmt := "Transcript show: "
				if isStringLiteral(op.Call.Args[0]) {
					stmt += args[0]
				} else {
					stmt += fmt.Sprintf("(%s) printString", args[0])
				}
				for i, a := range args[1:] {
					stmt += "; show: ' '; show: "
					if isStringLiteral(op.Call.Args[i+1]) {
						stmt += a
					} else {
						stmt += fmt.Sprintf("(%s) printString", a)
					}
				}
				val = stmt + "; cr"
			} else {
				call := val
				for _, a := range args {
					call += " value: " + a
				}
				val = call
			}
		case op.Index != nil:
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			val = fmt.Sprintf("%s at: %s", val, idx)
		case op.Field != nil:
			// treat field access like dictionary lookup
			val = fmt.Sprintf("%s at: %q", val, op.Field.Name)
		case op.Cast != nil:
			var tname string
			if op.Cast.Type.Simple != nil {
				tname = *op.Cast.Type.Simple
			}
			switch tname {
			case "int":
				val = fmt.Sprintf("%s asInteger", val)
			case "float":
				val = fmt.Sprintf("%s asFloat", val)
			case "string":
				val = fmt.Sprintf("%s asString", val)
			default:
				// unsupported cast, keep value as is
			}
		default:
			return "", fmt.Errorf("unsupported postfix expression")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Selector != nil:
		name := p.Selector.Root
		for _, s := range p.Selector.Tail {
			name += "." + s
		}
		return name, nil
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
	case p.Group != nil:
		return c.compileExpr(p.Group)
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
	case p.FunExpr != nil:
		params := make([]string, len(p.FunExpr.Params))
		for i, pa := range p.FunExpr.Params {
			params[i] = ":" + pa.Name
		}
		var body string
		if p.FunExpr.ExprBody != nil {
			b, err := c.compileExpr(p.FunExpr.ExprBody)
			if err != nil {
				return "", err
			}
			body = b
		} else {
			b, err := c.blockString(p.FunExpr.BlockBody)
			if err != nil {
				return "", err
			}
			body = b
		}
		return "[" + strings.Join(params, " ") + " | " + body + " ]", nil
	case p.If != nil:
		cond, err := c.compileExpr(p.If.Cond)
		if err != nil {
			return "", err
		}
		th, err := c.compileExpr(p.If.Then)
		if err != nil {
			return "", err
		}
		el := ""
		if p.If.Else != nil {
			el, err = c.compileExpr(p.If.Else)
			if err != nil {
				return "", err
			}
		}
		if el == "" {
			return fmt.Sprintf("(%s) ifTrue: [%s]", cond, th), nil
		}
		return fmt.Sprintf("(%s) ifTrue: [%s] ifFalse: [%s]", cond, th, el), nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		if p.Call.Func == "print" {
			if len(args) == 0 {
				return "", fmt.Errorf("print expects at least 1 arg")
			}
			stmt := "Transcript show: "
			if isStringLiteral(p.Call.Args[0]) {
				stmt += args[0]
			} else {
				stmt += fmt.Sprintf("(%s) printString", args[0])
			}
			for i, a := range args[1:] {
				stmt += "; show: ' '; show: "
				if isStringLiteral(p.Call.Args[i+1]) {
					stmt += a
				} else {
					stmt += fmt.Sprintf("(%s) printString", a)
				}
			}
			return stmt + "; cr", nil
		}
		call := p.Call.Func
		for _, a := range args {
			call += " value: " + a
		}
		return call, nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		s := strings.ReplaceAll(*l.Str, "'", "''")
		return fmt.Sprintf("'%s'", s)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	default:
		return "nil"
	}
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	return u.Value.Target != nil && u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil
}

func (c *Compiler) blockString(stmts []*parser.Statement) (string, error) {
	sub := &Compiler{}
	for _, st := range stmts {
		if err := sub.compileStmt(st); err != nil {
			return "", err
		}
	}
	return strings.TrimSpace(sub.buf.String()), nil
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
