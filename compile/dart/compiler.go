package dartcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Dart source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	tempVarCount int
	imports      map[string]bool
}

// New creates a new Dart compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, imports: make(map[string]bool)}
}

// Compile returns Dart source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.imports = make(map[string]bool)

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body

	// Emit function declarations first.
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Emit main function with remaining statements.
	c.writeln("void main() {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	bodyBytes := c.buf.Bytes()

	c.buf = oldBuf
	if len(c.imports) > 0 {
		for imp := range c.imports {
			c.writeln(fmt.Sprintf("import '%s';", imp))
		}
		c.writeln("")
	}
	c.buf.Write(bodyBytes)

	return c.buf.Bytes(), nil
}

// --- Statements ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + expr + ";")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	name := sanitizeName(s.Name)
	val := "0"
	if s.Value != nil {
		expr, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeln(fmt.Sprintf("var %s = %s;", name, val))
	return nil
}

func (c *Compiler) compileIf(s *parser.IfStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, st := range s.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if s.ElseIf != nil {
		c.writeln("} else ")
		return c.compileIf(s.ElseIf)
	}
	if len(s.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, st := range s.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	} else {
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileFor(s *parser.ForStmt) error {
	name := sanitizeName(s.Name)
	loopVar := name
	if s.Name == "_" {
		loopVar = c.newVar()
	}
	if s.RangeEnd != nil {
		start, err := c.compileExpr(s.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(s.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (var %s = %s; %s < %s; %s++) {", loopVar, start, loopVar, end, loopVar))
	} else {
		src, err := c.compileExpr(s.Source)
		if err != nil {
			return err
		}
		iterVar := src
		needTemp := isMapExpr(c, s.Source) || isStringExpr(c, s.Source)
		if needTemp {
			tmp := c.newVar()
			c.writeln(fmt.Sprintf("var %s = %s;", tmp, src))
			iterVar = tmp
		}
		if isMapExpr(c, s.Source) {
			c.writeln(fmt.Sprintf("for (var %s in %s.values) {", loopVar, iterVar))
		} else if isStringExpr(c, s.Source) {
			c.writeln(fmt.Sprintf("for (var %s in %s.runes) {", loopVar, iterVar))
		} else {
			c.writeln(fmt.Sprintf("for (var %s in %s) {", loopVar, iterVar))
		}
	}
	c.indent++
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	name := sanitizeName(s.Name)
	val := "null"
	if s.Value != nil {
		expr, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeln(fmt.Sprintf("var %s = %s;", name, val))
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	lhs := sanitizeName(s.Name)
	for _, idx := range s.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	rhs, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileWhile(s *parser.WhileStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, st := range s.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

// --- Expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", nil
	}
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	expr := left
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		if op.Op == "in" {
			if isMapPostfix(c, op.Right) {
				expr = fmt.Sprintf("(%s.containsKey(%s))", right, expr)
			} else {
				expr = fmt.Sprintf("(%s.contains(%s))", right, expr)
			}
		} else {
			expr = fmt.Sprintf("(%s %s %s)", expr, op.Op, right)
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
		val = fmt.Sprintf("%s%s", u.Ops[i], val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if p == nil {
		return "", nil
	}
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
			expr = fmt.Sprintf("%s[%s]", expr, idx)
		} else if op.Call != nil {
			call, err := c.compileCallOp(expr, op.Call)
			if err != nil {
				return "", err
			}
			expr = call
		}
	}
	return expr, nil
}

func (c *Compiler) compileCallOp(receiver string, call *parser.CallOp) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	return fmt.Sprintf("%s(%s)", receiver, strings.Join(args, ", ")), nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s: %s", k, v)
		}
		return "{" + strings.Join(items, ", ") + "}", nil
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	default:
		return "", fmt.Errorf("unsupported primary expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := sanitizeName(call.Func)
	// handle len()
	if name == "len" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s.length", arg), nil
	}
	// handle str()
	if name == "str" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s.toString()", arg), nil
	}
	// handle count()
	if name == "count" && len(call.Args) == 1 {
		argExpr := call.Args[0]
		arg, err := c.compileExpr(argExpr)
		if err != nil {
			return "", err
		}
		if isStringExpr(c, argExpr) {
			return fmt.Sprintf("%s.runes.length", arg), nil
		}
		return fmt.Sprintf("%s.length", arg), nil
	}
	// handle avg()
	if name == "avg" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("((){var _l=%s;var _s=0;for(var _x in _l){_s+=_x;}return _l.isEmpty?0:_s/_l.length;})()", arg), nil
	}
	// handle input()
	if name == "input" && len(call.Args) == 0 {
		c.imports["dart:io"] = true
		return "stdin.readLineSync() ?? ''", nil
	}
	// handle print with multiple arguments
	if name == "print" && len(call.Args) > 1 {
		parts := make([]string, len(call.Args))
		for i, a := range call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s.toString()", v)
		}
		return fmt.Sprintf("print([%s].join(' '))", strings.Join(parts, ", ")), nil
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileLiteral(lit *parser.Literal) (string, error) {
	switch {
	case lit.Int != nil:
		return strconv.Itoa(*lit.Int), nil
	case lit.Float != nil:
		return strconv.FormatFloat(*lit.Float, 'f', -1, 64), nil
	case lit.Str != nil:
		return strconv.Quote(*lit.Str), nil
	case lit.Bool != nil:
		if *lit.Bool {
			return "true", nil
		}
		return "false", nil
	}
	return "", fmt.Errorf("unknown literal")
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		params[i] = sanitizeName(p.Name)
	}
	ret := dartType(fun.Return)
	if ret == "" {
		c.writeln(fmt.Sprintf("%s(%s) {", name, strings.Join(params, ", ")))
	} else {
		c.writeln(fmt.Sprintf("%s %s(%s) {", ret, name, strings.Join(params, ", ")))
	}
	c.indent++
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func isMapExpr(c *Compiler, e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			if u.Value.Target.Map != nil {
				return true
			}
			if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
				if c.env != nil {
					if t, err := c.env.GetVar(sel.Root); err == nil {
						if _, ok := t.(types.MapType); ok {
							return true
						}
					}
				}
			}
		}
	}
	return false
}

func isStringExpr(c *Compiler, e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			if lit := u.Value.Target.Lit; lit != nil && lit.Str != nil {
				return true
			}
			if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
				if c.env != nil {
					if t, err := c.env.GetVar(sel.Root); err == nil {
						if _, ok := t.(types.StringType); ok {
							return true
						}
					}
				}
			}
		}
	}
	return false
}

func isMapPostfix(c *Compiler, p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}
	return isMapExpr(c, e)
}

func isStringPostfix(c *Compiler, p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}
	return isStringExpr(c, e)
}
