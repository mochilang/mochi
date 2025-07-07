//go:build slow

package racket

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

type Compiler struct {
	buf bytes.Buffer
}

func New() *Compiler { return &Compiler{} }

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("#lang racket")
	for _, st := range prog.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	if c.buf.Len() == 0 {
		c.writeln("")
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) write(s string)   { c.buf.WriteString(s) }
func (c *Compiler) writeln(s string) { c.buf.WriteString(s); c.buf.WriteByte('\n') }

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		name := s.Let.Name
		expr := "0"
		if s.Let.Value != nil {
			v, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			expr = v
		}
		c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
	case s.Assign != nil:
		if len(s.Assign.Index) > 0 || len(s.Assign.Field) > 0 {
			return fmt.Errorf("complex assignment not supported")
		}
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(set! %s %s)", s.Assign.Name, val))
	case s.Var != nil:
		name := s.Var.Name
		expr := "0"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			expr = v
		}
		c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(val)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Expr != nil:
		e, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(e)
	case s.For != nil:
		return c.compileFor(s.For)
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	val, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", err
	}
	isStrLeft := isStringUnary(e.Binary.Left)
	for _, op := range e.Binary.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightStr := isStringPostfix(op.Right)
		operator := op.Op
		switch operator {
		case "+":
			if isStrLeft || rightStr {
				val = fmt.Sprintf("(string-append %s %s)", val, rhs)
			} else {
				val = fmt.Sprintf("(+ %s %s)", val, rhs)
			}
		case "-", "*", "/":
			val = fmt.Sprintf("(%s %s %s)", operator, val, rhs)
		case "%":
			val = fmt.Sprintf("(remainder %s %s)", val, rhs)
		case "==":
			if isStrLeft || rightStr {
				val = fmt.Sprintf("(string=? %s %s)", val, rhs)
			} else {
				val = fmt.Sprintf("(= %s %s)", val, rhs)
			}
		case "!=":
			if isStrLeft || rightStr {
				val = fmt.Sprintf("(not (string=? %s %s))", val, rhs)
			} else {
				val = fmt.Sprintf("(not (= %s %s))", val, rhs)
			}
		case "<", "<=", ">", ">=":
			cmpOp := operator
			if isStrLeft || rightStr {
				switch operator {
				case "<":
					cmpOp = "string<?"
				case "<=":
					cmpOp = "string<=?"
				case ">":
					cmpOp = "string>?"
				case ">=":
					cmpOp = "string>=?"
				}
			}
			val = fmt.Sprintf("(%s %s %s)", cmpOp, val, rhs)
		case "&&":
			val = fmt.Sprintf("(and %s %s)", val, rhs)
		case "||":
			val = fmt.Sprintf("(or %s %s)", val, rhs)
		default:
			return "", fmt.Errorf("unsupported operator %s", operator)
		}
		isStrLeft = isStrLeft || rightStr
	}
	return val, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = fmt.Sprintf("(- %s)", val)
		case "!":
			val = fmt.Sprintf("(not %s)", val)
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
		case op.Cast != nil:
			if op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "int" {
				val = fmt.Sprintf("(string->number %s)", val)
			} else {
				return "", fmt.Errorf("unsupported cast")
			}
		default:
			return "", fmt.Errorf("unsupported postfix operation")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		switch p.Call.Func {
		case "print":
			if len(args) != 1 {
				return "", fmt.Errorf("print expects 1 arg")
			}
			return fmt.Sprintf("(displayln %s)", args[0]), nil
		case "append":
			if len(args) != 2 {
				return "", fmt.Errorf("append expects 2 args")
			}
			return fmt.Sprintf("(append %s (list %s))", args[0], args[1]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			tmp := args[0]
			return fmt.Sprintf("(let ([xs %s]) (/ (apply + xs) (length xs)))", tmp), nil
		case "sum":
			if len(args) != 1 {
				return "", fmt.Errorf("sum expects 1 arg")
			}
			return fmt.Sprintf("(apply + %s)", args[0]), nil
		case "len":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects 1 arg")
			}
			return fmt.Sprintf("(length %s)", args[0]), nil
		case "str":
			if len(args) != 1 {
				return "", fmt.Errorf("str expects 1 arg")
			}
			return fmt.Sprintf("(number->string %s)", args[0]), nil
		default:
			return fmt.Sprintf("(%s %s)", p.Call.Func, strings.Join(args, " ")), nil
		}
	case p.If != nil:
		cond, err := c.compileExpr(p.If.Cond)
		if err != nil {
			return "", err
		}
		thn, err := c.compileExpr(p.If.Then)
		if err != nil {
			return "", err
		}
		els, err := c.compileExpr(p.If.Else)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(if %s %s %s)", cond, thn, els), nil
	case p.Selector != nil:
		// simple variable access only
		return p.Selector.Root, nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return fmt.Sprintf("'(%s)", strings.Join(elems, " ")), nil
	case p.FunExpr != nil:
		params := make([]string, len(p.FunExpr.Params))
		for i, pa := range p.FunExpr.Params {
			params[i] = pa.Name
		}
		if p.FunExpr.ExprBody != nil {
			body, err := c.compileExpr(p.FunExpr.ExprBody)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(lambda (%s) %s)", strings.Join(params, " "), body), nil
		}
		var buf bytes.Buffer
		buf.WriteString(fmt.Sprintf("(lambda (%s)", strings.Join(params, " ")))
		for _, st := range p.FunExpr.BlockBody {
			if err := c.compileStmt(st); err != nil {
				return "", err
			}
		}
		buf.WriteString(")")
		return buf.String(), nil
	default:
		return "", fmt.Errorf("unsupported primary")
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%v", *l.Float), nil
	case l.Bool != nil:
		if *l.Bool {
			return "#t", nil
		}
		return "#f", nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Null:
		return "'()", nil
	default:
		return "", fmt.Errorf("unknown literal")
	}
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := f.Name
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end := ""
	if f.RangeEnd != nil {
		end, err = c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
	}
	if end != "" {
		c.writeln(fmt.Sprintf("(for ([%s (in-range %s %s)])", name, start, end))
	} else {
		c.writeln(fmt.Sprintf("(for ([%s %s])", name, start))
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln(")")
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = p.Name
	}
	c.writeln(fmt.Sprintf("(define (%s %s)", f.Name, strings.Join(params, " ")))
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln(")")
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(if %s", cond))
	c.writeln("  (begin")
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.writeln("  )")
	if st.ElseIf != nil {
		// else if handled recursively
		if err := c.compileIf(st.ElseIf); err != nil {
			return err
		}
	} else if len(st.Else) > 0 {
		c.writeln("  (begin")
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.writeln("  )")
	} else {
		c.writeln("  (void)")
	}
	c.writeln(")")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("(let loop ()")
	c.writeln(fmt.Sprintf("  (when %s", cond))
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("    (loop)))")
	return nil
}

func isStringPrimary(p *parser.Primary) bool {
	return p != nil && p.Lit != nil && p.Lit.Str != nil
}

func isStringPostfix(p *parser.PostfixExpr) bool {
	return isStringPrimary(p.Target)
}

func isStringUnary(u *parser.Unary) bool {
	return isStringPostfix(u.Value)
}
