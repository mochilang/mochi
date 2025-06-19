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
	useIndexStr  bool
}

// New creates a new Dart compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, imports: make(map[string]bool), useIndexStr: false}
}

// Compile returns Dart source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.imports = make(map[string]bool)

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body

	// Emit type declarations first.
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Emit function declarations next.
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
	c.emitRuntime()

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
	case s.Fun != nil:
		return c.compileFun(s.Fun)
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
	var typ types.Type
	if c.env != nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
	}
	if (typ == nil || isAny(typ)) && s.Value != nil {
		typ = c.inferExprType(s.Value)
	}
	typStr := dartType(typ)
	var val string
	if s.Value != nil {
		expr, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	if val == "" {
		if typStr != "" && typStr != "dynamic" {
			c.writeln(fmt.Sprintf("%s %s;", typStr, name))
		} else {
			c.writeln(fmt.Sprintf("var %s;", name))
		}
	} else {
		if typStr != "" && typStr != "dynamic" {
			c.writeln(fmt.Sprintf("%s %s = %s;", typStr, name, val))
		} else {
			c.writeln(fmt.Sprintf("var %s = %s;", name, val))
		}
	}
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
	var typ types.Type
	if c.env != nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
	}
	if (typ == nil || isAny(typ)) && s.Value != nil {
		typ = c.inferExprType(s.Value)
	}
	typStr := dartType(typ)
	var val string
	if s.Value != nil {
		expr, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	if val == "" {
		if typStr != "" && typStr != "dynamic" {
			c.writeln(fmt.Sprintf("%s %s;", typStr, name))
		} else {
			c.writeln(fmt.Sprintf("var %s;", name))
		}
	} else {
		if typStr != "" && typStr != "dynamic" {
			c.writeln(fmt.Sprintf("%s %s = %s;", typStr, name, val))
		} else {
			c.writeln(fmt.Sprintf("var %s = %s;", name, val))
		}
	}
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

	operands := []string{}
	operators := []string{}
	posts := []*parser.PostfixExpr{}

	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, left)

	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, right)
		operators = append(operators, op.Op)
		posts = append(posts, op.Right)
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

	for _, lvl := range levels {
		for i := 0; i < len(operators); {
			if containsOp(lvl, operators[i]) {
				l := operands[i]
				r := operands[i+1]
				op := operators[i]
				var expr string
				if op == "/" {
					op = "~/"
				}
				if op == "in" {
					if isMapPostfix(c, posts[i]) {
						expr = fmt.Sprintf("(%s.containsKey(%s))", r, l)
					} else {
						expr = fmt.Sprintf("(%s.contains(%s))", r, l)
					}
				} else {
					expr = fmt.Sprintf("(%s %s %s)", l, op, r)
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
				posts = append(posts[:i], posts[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected binary expression")
	}
	return operands[0], nil
}

func containsOp(list []string, op string) bool {
	for _, o := range list {
		if o == op {
			return true
		}
	}
	return false
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
			if op.Index.Colon != nil {
				start := "0"
				end := fmt.Sprintf("%s.length", expr)
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if isStringPrimary(c, p.Target) {
					expr = fmt.Sprintf("%s.substring(%s, %s)", expr, start, end)
				} else {
					expr = fmt.Sprintf("%s.sublist(%s, %s)", expr, start, end)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if isStringPrimary(c, p.Target) {
					c.useIndexStr = true
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
			}
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
			if c.env != nil {
				if t, err := c.env.GetVar(p.Selector.Root); err == nil {
					if _, ok := t.(types.MapType); ok {
						key := sanitizeName(p.Selector.Tail[0])
						name += fmt.Sprintf("['%s']", key)
						if len(p.Selector.Tail) > 1 {
							name += "." + strings.Join(p.Selector.Tail[1:], ".")
						}
						return name, nil
					}
				}
			}
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
			var k string
			if id, ok := identName(it.Key); ok {
				k = strconv.Quote(sanitizeName(id))
			} else {
				var err error
				k, err = c.compileExpr(it.Key)
				if err != nil {
					return "", err
				}
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s: %s", k, v)
		}
		return "{" + strings.Join(items, ", ") + "}", nil
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
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
		s := *lit.Str
		s = strings.ReplaceAll(s, "\\", "\\\\")
		s = strings.ReplaceAll(s, "\"", "\\\"")
		s = strings.ReplaceAll(s, "$", "\\$")
		return "\"" + s + "\"", nil
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

	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		}
	}
	if ft.Params == nil {
		ft.Params = make([]types.Type, len(fun.Params))
		for i, p := range fun.Params {
			if p.Type != nil {
				ft.Params[i] = c.resolveTypeRef(p.Type)
			} else {
				ft.Params[i] = types.AnyType{}
			}
		}
	}
	if ft.Return == nil {
		if fun.Return != nil {
			ft.Return = c.resolveTypeRef(fun.Return)
		} else {
			ft.Return = types.VoidType{}
		}
	}

	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		ptype := dartType(ft.Params[i])
		if ptype != "" && ptype != "dynamic" {
			params[i] = fmt.Sprintf("%s %s", ptype, sanitizeName(p.Name))
		} else {
			params[i] = sanitizeName(p.Name)
		}
	}

	c.writeln(fmt.Sprintf("%s %s(%s) {", dartType(ft.Return), name, strings.Join(params, ", ")))
	c.indent++
	origEnv := c.env
	child := types.NewEnv(c.env)
	for i, p := range fun.Params {
		child.SetVar(p.Name, ft.Params[i], true)
	}
	c.env = child
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.env = origEnv
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), expr), nil
	}
	sub := &Compiler{env: types.NewEnv(c.env), imports: c.imports}
	for _, p := range fn.Params {
		sub.env.SetVar(p.Name, types.AnyType{}, true)
	}
	for _, s := range fn.BlockBody {
		if err := sub.compileStmt(s); err != nil {
			return "", err
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	return fmt.Sprintf("(%s) {\n%s}", strings.Join(params, ", "), body), nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("abstract class %s {}", name))
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			c.writeln(fmt.Sprintf("class %s extends %s {", vname, name))
			c.indent++
			fields := []string{}
			for _, f := range v.Fields {
				fname := sanitizeName(f.Name)
				typ := dartType(c.resolveTypeRef(f.Type))
				if typ == "" {
					typ = "dynamic"
				}
				c.writeln(fmt.Sprintf("%s %s;", typ, fname))
				fields = append(fields, "this."+fname)
			}
			var ctor string
			if len(fields) == 0 {
				ctor = fmt.Sprintf("%s();", vname)
			} else {
				ctor = fmt.Sprintf("%s({%s});", vname, strings.Join(fields, ", "))
			}
			c.writeln(ctor)
			c.indent--
			c.writeln("}")
		}
		return nil
	}
	c.writeln(fmt.Sprintf("class %s {", name))
	c.indent++
	fields := []string{}
	for _, m := range t.Members {
		if m.Field != nil {
			fname := sanitizeName(m.Field.Name)
			typ := dartType(c.resolveTypeRef(m.Field.Type))
			if typ == "" {
				typ = "dynamic"
			}
			c.writeln(fmt.Sprintf("%s %s;", typ, fname))
			fields = append(fields, "this."+fname)
		}
	}
	var ctor string
	if len(fields) == 0 {
		ctor = fmt.Sprintf("%s();", name)
	} else {
		ctor = fmt.Sprintf("%s({%s});", name, strings.Join(fields, ", "))
	}
	c.writeln(ctor)
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
	}

	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		joinOns[i] = on
	}

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var where string
	if q.Where != nil {
		where, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	var sortExpr, skipExpr, takeExpr string
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}

	var b strings.Builder
	b.WriteString("(() {\n")
	b.WriteString("\tvar _res = [];\n")
	b.WriteString(fmt.Sprintf("\tfor (var %s in %s) {\n", sanitizeName(q.Var), src))
	indent := "\t\t"
	for i, fs := range fromSrcs {
		b.WriteString(fmt.Sprintf(indent+"for (var %s in %s) {\n", sanitizeName(q.Froms[i].Var), fs))
		indent += "\t"
	}
	for i, js := range joinSrcs {
		b.WriteString(fmt.Sprintf(indent+"for (var %s in %s) {\n", sanitizeName(q.Joins[i].Var), js))
		indent += "\t"
		b.WriteString(fmt.Sprintf(indent+"if (!(%s)) {\n", joinOns[i]))
		b.WriteString(indent + "\tcontinue;\n")
		b.WriteString(indent + "}\n")
	}
	if where != "" {
		b.WriteString(indent + "if (!(" + where + ")) {\n")
		b.WriteString(indent + "\tcontinue;\n")
		b.WriteString(indent + "}\n")
	}
	b.WriteString(fmt.Sprintf(indent+"_res.add(%s);\n", sel))
	for range joinSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	for range fromSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	b.WriteString("\t}\n")
	if sortExpr != "" || skipExpr != "" || takeExpr != "" {
		b.WriteString("\tvar items = List.from(_res);\n")
		if sortExpr != "" {
			v := sanitizeName(q.Var)
			b.WriteString(fmt.Sprintf("\titems.sort((%sA, %sB) {\n", v, v))
			b.WriteString(fmt.Sprintf("\t\tvar %s = %sA;\n", v, v))
			b.WriteString(fmt.Sprintf("\t\tvar keyA = %s;\n", sortExpr))
			b.WriteString(fmt.Sprintf("\t\t%s = %sB;\n", v, v))
			b.WriteString(fmt.Sprintf("\t\tvar keyB = %s;\n", sortExpr))
			b.WriteString("\t\treturn Comparable.compare(keyA, keyB);\n")
			b.WriteString("\t});\n")
		}
		if skipExpr != "" {
			b.WriteString(fmt.Sprintf("\tvar skip = %s;\n", skipExpr))
			b.WriteString("\tif (skip < items.length) {\n")
			b.WriteString("\t\titems = items.sublist(skip);\n")
			b.WriteString("\t} else {\n")
			b.WriteString("\t\titems = [];\n")
			b.WriteString("\t}\n")
		}
		if takeExpr != "" {
			b.WriteString(fmt.Sprintf("\tvar take = %s;\n", takeExpr))
			b.WriteString("\tif (take < items.length) {\n")
			b.WriteString("\t\titems = items.sublist(0, take);\n")
			b.WriteString("\t}\n")
		}
		b.WriteString("\t_res = items;\n")
	}
	b.WriteString("\treturn _res;\n")
	b.WriteString("})()")
	return b.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(() {\n")
	b.WriteString("\tvar _t = " + target + ";\n")
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\treturn " + res + ";\n")
			b.WriteString("})()")
			return b.String(), nil
		}
		cond := ""
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				cond = fmt.Sprintf("_t is %s", sanitizeName(call.Func))
				names := []string{}
				vals := []string{}
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						names = append(names, sanitizeName(id))
						field := sanitizeName(st.Order[idx])
						vals = append(vals, fmt.Sprintf("(_t as %s).%s", sanitizeName(call.Func), field))
					}
				}
				if len(names) > 0 {
					res = fmt.Sprintf("((%s) { return %s; })(%s)", strings.Join(names, ", "), res, strings.Join(vals, ", "))
				}
			}
		} else if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond = fmt.Sprintf("_t is %s", sanitizeName(ident))
			}
		}
		if cond == "" {
			pat, err := c.compileExpr(cs.Pattern)
			if err != nil {
				return "", err
			}
			cond = fmt.Sprintf("_t == %s", pat)
		}
		b.WriteString(fmt.Sprintf("\tif (%s) { return %s; }\n", cond, res))
	}
	b.WriteString("\treturn null;\n")
	b.WriteString("})()")
	return b.String(), nil
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

func isStringPrimary(c *Compiler, p *parser.Primary) bool {
	if p == nil {
		return false
	}
	e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: p}}}}
	return isStringExpr(c, e)
}

func (c *Compiler) emitRuntime() {
	if !c.useIndexStr {
		return
	}
	c.writeln("")
	c.writeln("String _indexString(String s, int i) {")
	c.indent++
	c.writeln("var runes = s.runes.toList();")
	c.writeln("if (i < 0) {")
	c.indent++
	c.writeln("i += runes.length;")
	c.indent--
	c.writeln("}")
	c.writeln("if (i < 0 || i >= runes.length) {")
	c.indent++
	c.writeln("throw RangeError('index out of range');")
	c.indent--
	c.writeln("}")
	c.writeln("return String.fromCharCode(runes[i]);")
	c.indent--
	c.writeln("}")
}
