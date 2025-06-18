package cljcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Clojure source code (limited subset).
type Compiler struct {
	buf       bytes.Buffer
	indent    int
	env       *types.Env
	mainStmts []*parser.Statement
}

// New creates a new Clojure compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

// Compile generates Clojure code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		} else {
			c.mainStmts = append(c.mainStmts, s)
		}
	}
	for _, s := range c.mainStmts {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	c.writeIndent()
	c.buf.WriteString("(defn " + sanitizeName(fn.Name) + " [")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(" ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
	}
	c.buf.WriteString("]\n")
	c.indent++
	c.writeln("(try")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("(catch clojure.lang.ExceptionInfo e")
	c.indent++
	c.writeln("(if (= (.getMessage e) \"return\")")
	c.indent++
	c.writeln("(:value (ex-data e))")
	c.indent--
	c.writeln("(throw e)))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if expr != "" {
			c.writeln(expr)
		}
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(st *parser.LetStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(def %s %s)", sanitizeName(st.Name), expr))
	return nil
}

func (c *Compiler) compileVar(st *parser.VarStmt) error {
	expr := "nil"
	if st.Value != nil {
		v, err := c.compileExpr(st.Value)
		if err != nil {
			return err
		}
		expr = v
	}
	c.writeln(fmt.Sprintf("(def %s %s)", sanitizeName(st.Name), expr))
	return nil
}

func (c *Compiler) compileAssign(st *parser.AssignStmt) error {
	name := sanitizeName(st.Name)
	rhs, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	if len(st.Index) == 0 {
		c.writeln(fmt.Sprintf("(def %s %s)", name, rhs))
		return nil
	}
	idxs := make([]string, len(st.Index))
	for i, idx := range st.Index {
		v, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		idxs[i] = v
	}
	if len(idxs) == 1 {
		c.writeln(fmt.Sprintf("(def %s (assoc %s %s %s))", name, name, idxs[0], rhs))
	} else {
		c.writeln(fmt.Sprintf("(def %s (assoc-in %s [%s] %s))", name, name, strings.Join(idxs, " "), rhs))
	}
	return nil
}

func (c *Compiler) compileReturn(st *parser.ReturnStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	if expr == "" {
		expr = "nil"
	}
	c.writeln(fmt.Sprintf("(throw (ex-info \"return\" {:value %s}))", expr))
	return nil
}

func (c *Compiler) compileFor(st *parser.ForStmt) error {
	name := sanitizeName(st.Name)
	if st.RangeEnd != nil {
		start, err := c.compileExpr(st.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(st.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(loop [%s %s]", name, start))
		c.indent++
		c.writeln(fmt.Sprintf("(when (< %s %s)", name, end))
		c.indent++
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.writeln(fmt.Sprintf("(recur (inc %s)))", name))
		c.indent--
		c.indent--
		c.writeln(")")
		return nil
	}
	src, err := c.compileExpr(st.Source)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(doseq [%s %s]", name, src))
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("(while " + cond)
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("(if " + cond)
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	if len(st.Else) > 0 {
		c.indent--
		c.writeIndent()
		c.buf.WriteString("\n")
		c.writeln("(do")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	}
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	if len(b.Right) == 0 {
		return left, nil
	}
	expr := left
	for _, op := range b.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		opName := op.Op
		switch opName {
		case "%":
			opName = "mod"
		case "&&":
			opName = "and"
		case "||":
			opName = "or"
		case "!=":
			opName = "not="
		}
		expr = fmt.Sprintf("(%s %s %s)", opName, expr, rhs)
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		name := op
		if name == "!" {
			name = "not"
		}
		expr = fmt.Sprintf("(%s %s)", name, expr)
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("(nth %s %s)", expr, idx)
			continue
		}
		if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, v)
			}
			name := expr
			if name == "print" {
				expr = fmt.Sprintf("(println %s)", strings.Join(args, " "))
			} else if name == "len" {
				expr = fmt.Sprintf("(count %s)", args[0])
			} else {
				expr = fmt.Sprintf("(%s %s)", name, strings.Join(args, " "))
			}
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, " ") + "]", nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return sanitizeName(p.Selector.Root), nil
		}
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, v)
		}
		name := sanitizeName(p.Call.Func)
		if name == "print" {
			return "(println " + strings.Join(args, " ") + ")", nil
		}
		if name == "len" && len(args) == 1 {
			return "(count " + args[0] + ")", nil
		}
		return "(" + name + " " + strings.Join(args, " ") + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
}
