package stcode

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into GNU Smalltalk source code.
type Compiler struct {
	buf       bytes.Buffer
	indent    int
	env       *types.Env
	funParams map[string][]string
}

// New creates a new Smalltalk compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env, funParams: make(map[string][]string)} }

func (c *Compiler) writeln(s string)         { c.writeIndent(); c.buf.WriteString(s); c.buf.WriteByte('\n') }
func (c *Compiler) writelnNoIndent(s string) { c.buf.WriteString(s); c.buf.WriteByte('\n') }
func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

// Compile generates Smalltalk code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!")
	c.writeln("")
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	c.writelnNoIndent("!!")
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	header := fn.Name + ": " + fn.Params[0].Name
	names := []string{fn.Params[0].Name}
	for _, p := range fn.Params[1:] {
		header += " " + p.Name + ": " + p.Name
		names = append(names, p.Name)
	}
	c.funParams[fn.Name] = names
	vars := collectVars(fn.Body)
	c.writeln("!Main class methodsFor: 'mochi'!")
	if len(vars) > 0 {
		c.writeln(header + " | " + strings.Join(vars, " ") + " |")
	} else {
		c.writeln(header)
	}
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writelnNoIndent("!")
	return nil
}

func collectVars(stmts []*parser.Statement) []string {
	set := map[string]bool{}
	var visit func([]*parser.Statement)
	visit = func(list []*parser.Statement) {
		for _, s := range list {
			switch {
			case s.Let != nil:
				set[s.Let.Name] = true
			case s.Var != nil:
				set[s.Var.Name] = true
			case s.For != nil:
				set[s.For.Name] = true
				visit(s.For.Body)
			case s.While != nil:
				visit(s.While.Body)
			case s.If != nil:
				visit(s.If.Then)
				if s.If.ElseIf != nil {
					visit(s.If.ElseIf.Then)
				}
				visit(s.If.Else)
			}
		}
	}
	visit(stmts)
	vars := make([]string, 0, len(set))
	for v := range set {
		vars = append(vars, v)
	}
	sort.Strings(vars)
	return vars
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := %s.", s.Let.Name, val))
	case s.Var != nil:
		if err := c.compileVar(s.Var); err != nil {
			return err
		}
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("^ " + val)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if expr != "" {
			c.writeln(expr + ".")
		}
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	if f.RangeEnd == nil {
		return fmt.Errorf("collection loops not supported")
	}
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(f.RangeEnd)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s to: %s - 1 do: [:%s |", start, end, f.Name))
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("]")
	c.writeln(".")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("[" + cond + "] whileTrue: [")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("]")
	c.writeln(".")
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	if len(a.Index) > 0 {
		return fmt.Errorf("index assignment not supported")
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s.", a.Name, val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	val := "nil"
	if v.Value != nil {
		expr, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeln(fmt.Sprintf("%s := %s.", v.Name, val))
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("(" + cond + ") ifTrue: [")
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if len(stmt.Else) > 0 {
		c.writeln("] ifFalse: [")
		c.indent++
		for _, st := range stmt.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("]")
		c.writeln(".")
	} else {
		c.writeln("]")
		c.writeln(".")
	}
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
	expr := left
	leftList := isListUnary(b.Left, c.env)
	leftStr := isStringUnary(b.Left, c.env)
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rlist := isListPostfix(op.Right, c.env)
		rstr := isStringPostfix(op.Right, c.env)
		switch op.Op {
		case "+":
			if leftList || rlist {
				expr = fmt.Sprintf("(%s , %s)", expr, right)
				leftList = true
				leftStr = false
			} else if leftStr || rstr {
				expr = fmt.Sprintf("(%s , %s)", expr, right)
				leftStr = true
				leftList = false
			} else {
				expr = fmt.Sprintf("(%s + %s)", expr, right)
				leftList = false
				leftStr = false
			}
		default:
			expr = fmt.Sprintf("(%s %s %s)", expr, mapOp(op.Op), right)
			leftList = false
			leftStr = false
		}
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	if u == nil {
		return "", nil
	}
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "!":
			val = fmt.Sprintf("(%s) not", val)
		default:
			val = fmt.Sprintf("%s%s", u.Ops[i], val)
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon != nil {
				start := "1"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = fmt.Sprintf("(%s + 1)", s)
				}
				end := fmt.Sprintf("%s size", expr)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				expr = fmt.Sprintf("(%s copyFrom: %s to: %s)", expr, start, end)
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("(%s at: %s + 1)", expr, idx)
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.List != nil:
		if len(p.List.Elems) == 0 {
			return "Array new", nil
		}
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "Array with: " + strings.Join(elems, " with: "), nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	case p.Selector != nil:
		return p.Selector.Root, nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := call.Func
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	switch name {
	case "print":
		if len(args) != 1 {
			return "", fmt.Errorf("print expects 1 arg")
		}
		return fmt.Sprintf("%s displayOn: Transcript. Transcript cr", args[0]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("%s size", args[0]), nil
	default:
		params, ok := c.funParams[name]
		if !ok {
			return "", fmt.Errorf("unsupported call %s", name)
		}
		parts := []string{"Main", name + ":"}
		for i, p := range params {
			arg := fmt.Sprintf("(%s)", args[i])
			if i == 0 {
				parts = append(parts, arg)
			} else {
				parts = append(parts, fmt.Sprintf("%s: %s", p, arg))
			}
		}
		return strings.Join(parts, " "), nil
	}
}

func mapOp(op string) string {
	switch op {
	case "==":
		return "="
	case "!=":
		return "~="
	case "%":
		return "\\"
	case "&&":
		return "&"
	case "||":
		return "|"
	}
	return op
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		s := strings.ReplaceAll(*l.Str, "'", "''")
		return "'" + s + "'", nil
	}
	return "", fmt.Errorf("unknown literal")
}

func isListUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isListPostfix(u.Value, env)
}

func isListPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if p.Target.List != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
	}
	return false
}

func isStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isStringPostfix(u.Value, env)
}

func isStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}
