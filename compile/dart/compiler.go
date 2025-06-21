package dartcode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
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
	packages     map[string]bool
	structs      map[string]bool
	handlerCount int
	useIndexStr  bool
	useUnionAll  bool
	useUnion     bool
	useExcept    bool
	useIntersect bool
	useFetch     bool
	useLoad      bool
	useSave      bool
	useGenText   bool
	useGenEmbed  bool
	useGenStruct bool
	useGroup     bool
	useGroupBy   bool
	useStream    bool
	useQuery     bool
}

// New creates a new Dart compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, imports: make(map[string]bool), packages: make(map[string]bool),
		structs: make(map[string]bool), useIndexStr: false,
		useUnionAll: false, useUnion: false, useExcept: false, useIntersect: false,
		useFetch: false, useLoad: false, useSave: false,
		useGenText: false, useGenEmbed: false, useGenStruct: false,
		useGroup: false, useGroupBy: false, useStream: false,
		useQuery: false}
}

// Compile returns Dart source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.imports = make(map[string]bool)
	c.packages = make(map[string]bool)
	c.structs = make(map[string]bool)
	c.useIndexStr = false
	c.useUnionAll = false
	c.useUnion = false
	c.useExcept = false
	c.useIntersect = false
	c.useFetch = false
	c.useLoad = false
	c.useSave = false
	c.useGenText = false
	c.useGenEmbed = false
	c.useGenStruct = false
	c.useGroup = false
	c.useGroupBy = false
	c.useStream = false
	c.useQuery = false
	c.handlerCount = 0

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

	// Emit test block declarations.
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
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
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(fmt.Sprintf("%s();", name))
		}
	}
	c.indent--
	c.writeln("}")
	bodyBytes := c.buf.Bytes()

	c.buf = oldBuf
	if prog.Package != "" {
		c.writeln("library " + sanitizeName(prog.Package) + ";")
		c.writeln("")
	}
	if len(c.imports) > 0 {
		for imp := range c.imports {
			c.writeln(fmt.Sprintf("import '%s';", imp))
		}
		c.writeln("")
	}
	c.buf.Write(bodyBytes)
	c.emitRuntime()
	c.buf.WriteByte('\n')
	return c.buf.Bytes(), nil
}

// --- Statements ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
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
	case s.Stream != nil:
		return c.compileStreamDecl(s.Stream)
	case s.On != nil:
		return c.compileOnHandler(s.On)
	case s.Emit != nil:
		return c.compileEmit(s.Emit)
	case s.Import != nil:
		if s.Import.Lang == nil {
			return c.compilePackageImport(s.Import)
		}
		return fmt.Errorf("unsupported import language: %v", s.Import.Lang)
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
	if s.Type != nil {
		typ = c.resolveTypeRef(s.Type)
	}
	if c.env != nil && typ == nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
	}
	if (typ == nil || isAny(typ)) && s.Value != nil {
		typ = c.inferExprType(s.Value)
	}
	var val string
	if s.Value != nil {
		expr, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeVarDecl(name, typ, val)
	if c.env != nil && typ != nil && !isAny(typ) {
		c.env.SetVar(s.Name, typ, false)
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
			tmp := c.newVar()
			c.writeln(fmt.Sprintf("for (var %s in %s.runes) {", tmp, iterVar))
			if s.Name != "_" {
				c.indent++
				c.writeln(fmt.Sprintf("var %s = String.fromCharCode(%s);", loopVar, tmp))
				c.indent--
			}
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
	if s.Type != nil {
		typ = c.resolveTypeRef(s.Type)
	}
	if c.env != nil && typ == nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			typ = t
		}
	}
	if (typ == nil || isAny(typ)) && s.Value != nil {
		typ = c.inferExprType(s.Value)
	}
	var val string
	if s.Value != nil {
		expr, err := c.compileExpr(s.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeVarDecl(name, typ, val)
	if c.env != nil && typ != nil && !isAny(typ) {
		c.env.SetVar(s.Name, typ, true)
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
	if c.env != nil {
		if t, err := c.env.GetVar(s.Name); err == nil {
			if isInt(t) || isInt64(t) {
				if !isIntExpr(c, s.Value) {
					rhs = fmt.Sprintf("(%s).toInt()", rhs)
				}
			}
		}
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileStreamDecl(s *parser.StreamDecl) error {
	st, ok := c.env.GetStream(s.Name)
	if !ok {
		return fmt.Errorf("unknown stream: %s", s.Name)
	}
	c.compileStructType(st)
	varName := "_" + sanitizeName(s.Name) + "Stream"
	c.writeln(fmt.Sprintf("var %s = _Stream<%s>('%s');", varName, sanitizeName(st.Name), s.Name))
	c.useStream = true
	return nil
}

func (c *Compiler) compileOnHandler(h *parser.OnHandler) error {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", h.Stream)
	}
	c.compileStructType(st)
	streamVar := "_" + sanitizeName(h.Stream) + "Stream"
	handlerName := fmt.Sprintf("_handler_%d", c.handlerCount)
	c.handlerCount++
	c.writeln(fmt.Sprintf("void %s(%s ev) {", handlerName, sanitizeName(st.Name)))
	c.indent++
	alias := sanitizeName(h.Alias)
	c.writeln(fmt.Sprintf("var %s = ev;", alias))
	for _, stmt := range h.Body {
		if err := c.compileStmt(stmt); err != nil {
			c.indent--
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.writeln(fmt.Sprintf("%s.register(%s);", streamVar, handlerName))
	c.useStream = true
	return nil
}

func (c *Compiler) compileEmit(e *parser.EmitStmt) error {
	st, ok := c.env.GetStream(e.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", e.Stream)
	}
	c.compileStructType(st)
	parts := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
	}
	lit := fmt.Sprintf("%s({%s})", sanitizeName(st.Name), strings.Join(parts, ", "))
	streamVar := "_" + sanitizeName(e.Stream) + "Stream"
	c.writeln(fmt.Sprintf("%s.append(%s);", streamVar, lit))
	c.useStream = true
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
	floats := []bool{}

	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, left)
	floats = append(floats, isFloatUnary(c, b.Left))

	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, right)
		operators = append(operators, op.Op)
		posts = append(posts, op.Right)
		floats = append(floats, isFloatPostfix(c, op.Right))
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
				lf := floats[i]
				rf := floats[i+1]
				if op == "/" {
					if lf || rf {
						op = "/"
						floats[i] = true
					} else {
						op = "~/"
						floats[i] = false
					}
				} else if op == "*" || op == "+" || op == "-" {
					if lf || rf {
						floats[i] = true
					} else {
						floats[i] = false
					}
				} else {
					floats[i] = false
				}
				if op == "in" {
					if isMapPostfix(c, posts[i]) {
						expr = fmt.Sprintf("(%s.containsKey(%s))", r, l)
					} else {
						expr = fmt.Sprintf("(%s.contains(%s))", r, l)
					}
				} else if op == "union" {
					c.useUnion = true
					expr = fmt.Sprintf("_union(%s, %s)", l, r)
				} else if op == "union_all" {
					c.useUnionAll = true
					expr = fmt.Sprintf("_unionAll(%s, %s)", l, r)
				} else if op == "except" {
					c.useExcept = true
					expr = fmt.Sprintf("_except(%s, %s)", l, r)
				} else if op == "intersect" {
					c.useIntersect = true
					expr = fmt.Sprintf("_intersect(%s, %s)", l, r)
				} else {
					expr = fmt.Sprintf("(%s %s %s)", l, op, r)
				}
				operands[i] = expr
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
				posts = append(posts[:i], posts[i+1:]...)
				floats = append(floats[:i+1], floats[i+2:]...)
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
					if !isIntExpr(c, op.Index.Start) {
						idx = fmt.Sprintf("(%s).toInt()", idx)
					}
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
		} else if op.Cast != nil {
			t := c.resolveTypeRef(op.Cast.Type)
			dt := dartType(t)
			switch dt {
			case "double":
				expr = fmt.Sprintf("(%s).toDouble()", expr)
			case "int":
				expr = fmt.Sprintf("(%s).toInt()", expr)
			default:
				expr = fmt.Sprintf("(%s as %s)", expr, dt)
			}
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
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
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
		typStr := dartType(ft.Params[i])
		if typStr != "dynamic" && typStr != "" {
			params[i] = fmt.Sprintf("%s %s", typStr, sanitizeName(p.Name))
		} else {
			params[i] = sanitizeName(p.Name)
		}
	}

	ret := "dynamic"
	if rt := dartType(ft.Return); rt != "dynamic" && rt != "" {
		ret = rt
	}

	c.writeln(fmt.Sprintf("%s %s(%s) {", ret, name, strings.Join(params, ", ")))
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

func (c *Compiler) compileMethod(structName string, fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)

	var ft types.FuncType
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			if m, ok := st.Methods[fun.Name]; ok {
				ft = m.Type
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
		typStr := dartType(ft.Params[i])
		if typStr != "dynamic" && typStr != "" {
			params[i] = fmt.Sprintf("%s %s", typStr, sanitizeName(p.Name))
		} else {
			params[i] = sanitizeName(p.Name)
		}
	}

	ret := "dynamic"
	if rt := dartType(ft.Return); rt != "dynamic" && rt != "" {
		ret = rt
	}

	c.writeln(fmt.Sprintf("%s %s(%s) {", ret, name, strings.Join(params, ", ")))
	c.indent++
	origEnv := c.env
	child := types.NewEnv(c.env)
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			for fname, t := range st.Fields {
				child.SetVar(fname, t, true)
			}
		}
	}
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

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("class %s {", name))
	c.indent++
	fields := []string{}
	for _, fn := range st.Order {
		typ := dartType(st.Fields[fn])
		if typ == "" {
			typ = "dynamic"
		}
		fname := sanitizeName(fn)
		c.writeln(fmt.Sprintf("%s %s;", typ, fname))
		param := "this." + fname
		if typ != "dynamic" {
			param = "required " + param
		}
		fields = append(fields, param)
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
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("void %s() {", name))
	c.indent++
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (!(%s)) { throw Exception('expect failed'); }", expr))
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
				param := "this." + fname
				if typ != "dynamic" {
					param = "required " + param
				}
				fields = append(fields, param)
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
	var methods []*parser.FunStmt
	for _, m := range t.Members {
		if m.Field != nil {
			fname := sanitizeName(m.Field.Name)
			typ := dartType(c.resolveTypeRef(m.Field.Type))
			if typ == "" {
				typ = "dynamic"
			}
			c.writeln(fmt.Sprintf("%s %s;", typ, fname))
			param := "this." + fname
			if typ != "dynamic" {
				param = "required " + param
			}
			fields = append(fields, param)
		} else if m.Method != nil {
			methods = append(methods, m.Method)
		}
	}
	var ctor string
	if len(fields) == 0 {
		ctor = fmt.Sprintf("%s();", name)
	} else {
		ctor = fmt.Sprintf("%s({%s});", name, strings.Join(fields, ", "))
	}
	c.writeln(ctor)
	for _, m := range methods {
		if err := c.compileMethod(name, m); err != nil {
			return err
		}
		c.writeln("")
	}
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
	joinSides := make([]string, len(q.Joins))
	needsHelper := false
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
		if j.Side != nil {
			joinSides[i] = *j.Side
			needsHelper = true
		}
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

	v := sanitizeName(q.Var)
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		keyExpr, err := c.compileExpr(q.Group.Expr)
		if err != nil {
			return "", err
		}
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		c.useGroup = true
		c.useGroupBy = true
		expr := fmt.Sprintf("_group_by(%s, (%s) => %s).map((%s) => %s).toList()", src, v, keyExpr, sanitizeName(q.Group.Name), valExpr)
		return expr, nil
	}

	if needsHelper {
		varNames := []string{sanitizeName(q.Var)}
		for _, f := range q.Froms {
			varNames = append(varNames, sanitizeName(f.Var))
		}
		params := append([]string(nil), varNames...)
		joins := make([]string, 0, len(q.Froms)+len(q.Joins))
		for _, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("{'items': %s}", fs))
		}
		for i, js := range joinSrcs {
			onParams := append(params, sanitizeName(q.Joins[i].Var))
			onFn := fmt.Sprintf("(%s) => %s", strings.Join(onParams, ", "), joinOns[i])
			spec := fmt.Sprintf("{'items': %s, 'on': %s", js, onFn)
			if joinSides[i] == "left" || joinSides[i] == "outer" {
				spec += ", 'left': true"
			}
			if joinSides[i] == "right" || joinSides[i] == "outer" {
				spec += ", 'right': true"
			}
			spec += "}"
			joins = append(joins, spec)
			params = append(params, sanitizeName(q.Joins[i].Var))
		}
		allParams := strings.Join(params, ", ")
		selectFn := fmt.Sprintf("(%s) => %s", allParams, sel)
		var whereFn, sortFn string
		if where != "" {
			whereFn = fmt.Sprintf("(%s) => %s", allParams, where)
		}
		if sortExpr != "" {
			sortFn = fmt.Sprintf("(%s) => %s", allParams, sortExpr)
		}
		c.useQuery = true
		var buf strings.Builder
		buf.WriteString("(() {\n")
		buf.WriteString(fmt.Sprintf("\tvar src = %s;\n", src))
		buf.WriteString("\tvar res = _query(src, [\n")
		for _, j := range joins {
			buf.WriteString("\t\t" + j + ",\n")
		}
		buf.WriteString("\t], { 'select': " + selectFn)
		if whereFn != "" {
			buf.WriteString(", 'where': " + whereFn)
		}
		if sortFn != "" {
			buf.WriteString(", 'sortKey': " + sortFn)
		}
		if skipExpr != "" {
			buf.WriteString(", 'skip': " + skipExpr)
		}
		if takeExpr != "" {
			buf.WriteString(", 'take': " + takeExpr)
		}
		buf.WriteString(" });\n")
		buf.WriteString("\treturn res;\n")
		buf.WriteString("})()")
		return buf.String(), nil
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

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var (
		prompt string
		text   string
		model  string
		params []string
	)
	for _, f := range g.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		switch f.Name {
		case "prompt":
			prompt = v
		case "text":
			text = v
		case "model":
			model = v
		default:
			params = append(params, fmt.Sprintf("%q: %s", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramStr := "null"
	if len(params) > 0 {
		paramStr = fmt.Sprintf("{ %s }", strings.Join(params, ", "))
	}
	if model == "" {
		model = "\"\""
	}
	c.imports["dart:convert"] = true
	if g.Target == "embedding" {
		c.useGenEmbed = true
		return fmt.Sprintf("_genEmbed(%s, %s, %s)", text, model, paramStr), nil
	}
	if _, ok := c.env.GetStruct(g.Target); ok {
		c.useGenStruct = true
		return fmt.Sprintf("_genStruct<%s>(%s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
	}
	c.useGenText = true
	return fmt.Sprintf("_genText(%s, %s, %s)", prompt, model, paramStr), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	urlStr, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "null"
	if f.With != nil {
		w, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = w
	}
	c.imports["dart:io"] = true
	c.imports["dart:convert"] = true
	c.useFetch = true
	return fmt.Sprintf("_fetch(%s, %s)", urlStr, opts), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "null"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "null"
	if l.With != nil {
		w, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = w
	}
	c.imports["dart:io"] = true
	c.useLoad = true
	return fmt.Sprintf("_load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "null"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "null"
	if s.With != nil {
		w, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = w
	}
	c.imports["dart:io"] = true
	c.useSave = true
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compilePackageImport(im *parser.ImportStmt) error {
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = sanitizeName(alias)
	if c.packages[alias] {
		return nil
	}
	c.packages[alias] = true

	path := strings.Trim(im.Path, "\"")
	base := ""
	if strings.HasPrefix(path, "./") || strings.HasPrefix(path, "../") {
		base = filepath.Dir(im.Pos.Filename)
	}
	target := filepath.Join(base, path)
	info, err := os.Stat(target)
	if err != nil {
		if os.IsNotExist(err) && !strings.HasSuffix(target, ".mochi") {
			if fi, err2 := os.Stat(target + ".mochi"); err2 == nil {
				info = fi
				target += ".mochi"
			} else {
				return fmt.Errorf("import package: %w", err)
			}
		} else {
			return fmt.Errorf("import package: %w", err)
		}
	}
	var files []string
	if info.IsDir() {
		entries, err := os.ReadDir(target)
		if err != nil {
			return fmt.Errorf("import package: %w", err)
		}
		for _, e := range entries {
			if !e.IsDir() && strings.HasSuffix(e.Name(), ".mochi") {
				files = append(files, filepath.Join(target, e.Name()))
			}
		}
		sort.Strings(files)
	} else {
		files = []string{target}
	}

	pkgEnv := types.NewEnv(c.env)
	origEnv := c.env
	c.env = pkgEnv
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			c.env = origEnv
			return err
		}
		if errs := types.Check(prog, pkgEnv); len(errs) > 0 {
			c.env = origEnv
			return errs[0]
		}
		for _, s := range prog.Statements {
			if s.Fun != nil && s.Fun.Export {
				name := s.Fun.Name
				s.Fun.Name = alias + "_" + name
				if err := c.compileFun(s.Fun); err != nil {
					c.env = origEnv
					return err
				}
				c.writeln("")
			}
		}
	}
	c.env = origEnv
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

func isFloatExpr(c *Compiler, e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		return isFloatUnary(c, e.Binary.Left)
	}
	return false
}

func isFloatUnary(c *Compiler, u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isFloatPostfix(c, u.Value)
}

func isFloatPostfix(c *Compiler, p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		if last := p.Ops[len(p.Ops)-1]; last.Cast != nil {
			t := c.resolveTypeRef(last.Cast.Type)
			if isFloat(t) {
				return true
			}
		}
	}
	return isFloatPrimary(c, p.Target)
}

func isFloatPrimary(c *Compiler, p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if lit := p.Lit; lit != nil && lit.Float != nil {
		return true
	}
	if sel := p.Selector; sel != nil && len(sel.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(sel.Root); err == nil {
				if isFloat(t) {
					return true
				}
			}
		}
	}
	return false
}

func isIntExpr(c *Compiler, e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		return isIntUnary(c, e.Binary.Left)
	}
	return false
}

func isIntUnary(c *Compiler, u *parser.Unary) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isIntPostfix(c, u.Value)
}

func isIntPostfix(c *Compiler, p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if len(p.Ops) > 0 {
		if last := p.Ops[len(p.Ops)-1]; last.Cast != nil {
			t := c.resolveTypeRef(last.Cast.Type)
			if isInt(t) || isInt64(t) {
				return true
			}
		}
	}
	return isIntPrimary(c, p.Target)
}

func isIntPrimary(c *Compiler, p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if lit := p.Lit; lit != nil && lit.Int != nil {
		return true
	}
	if sel := p.Selector; sel != nil && len(sel.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(sel.Root); err == nil {
				if isInt(t) || isInt64(t) {
					return true
				}
			}
		}
	}
	return false
}

func isStringPrimary(c *Compiler, p *parser.Primary) bool {
	if p == nil {
		return false
	}
	e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: p}}}}
	return isStringExpr(c, e)
}

func (c *Compiler) emitRuntime() {
	if !(c.useIndexStr || c.useUnionAll || c.useUnion || c.useExcept || c.useIntersect || c.useFetch || c.useLoad || c.useSave || c.useGenText || c.useGenEmbed || c.useGenStruct || c.useGroup || c.useGroupBy || c.useStream || c.useQuery) {
		return
	}
	c.writeln("")
	if c.useIndexStr {
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
	if c.useUnionAll {
		c.writeln("List<dynamic> _unionAll(List<dynamic> a, List<dynamic> b) => [...a, ...b];")
	}
	if c.useUnion {
		c.writeln("List<dynamic> _union(List<dynamic> a, List<dynamic> b) {")
		c.indent++
		c.writeln("var res = [...a];")
		c.writeln("for (var it in b) {")
		c.indent++
		c.writeln("if (!res.contains(it)) {")
		c.indent++
		c.writeln("res.add(it);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.useExcept {
		c.writeln("List<dynamic> _except(List<dynamic> a, List<dynamic> b) {")
		c.indent++
		c.writeln("var res = <dynamic>[];")
		c.writeln("for (var it in a) {")
		c.indent++
		c.writeln("if (!b.contains(it)) {")
		c.indent++
		c.writeln("res.add(it);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.useIntersect {
		c.writeln("List<dynamic> _intersect(List<dynamic> a, List<dynamic> b) {")
		c.indent++
		c.writeln("var res = <dynamic>[];")
		c.writeln("for (var it in a) {")
		c.indent++
		c.writeln("if (b.contains(it) && !res.contains(it)) {")
		c.indent++
		c.writeln("res.add(it);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.useStream {
		c.writeln("class _Stream<T> {")
		c.indent++
		c.writeln("String name;")
		c.writeln("List<void Function(T)> handlers = [];")
		c.writeln("_Stream(this.name);")
		c.writeln("void append(T data) {")
		c.indent++
		c.writeln("for (var h in List.from(handlers)) { h(data); }")
		c.indent--
		c.writeln("}")
		c.writeln("void register(void Function(T) handler) { handlers.add(handler); }")
		c.indent--
		c.writeln("}")
		c.writeln("void _waitAll() {}")
	}
	if c.useGroup {
		c.writeln("class _Group {")
		c.indent++
		c.writeln("dynamic key;")
		c.writeln("List<dynamic> Items = [];")
		c.writeln("_Group(this.key);")
		c.indent--
		c.writeln("}")
	}
	if c.useGroupBy {
		c.writeln("List<_Group> _group_by(List<dynamic> src, dynamic Function(dynamic) keyfn) {")
		c.indent++
		c.writeln("var groups = <String,_Group>{};")
		c.writeln("var order = <String>[];")
		c.writeln("for (var it in src) {")
		c.indent++
		c.writeln("var key = keyfn(it);")
		c.writeln("var ks = key.toString();")
		c.writeln("var g = groups[ks];")
		c.writeln("if (g == null) {")
		c.indent++
		c.writeln("g = _Group(key);")
		c.writeln("groups[ks] = g;")
		c.writeln("order.add(ks);")
		c.indent--
		c.writeln("}")
		c.writeln("g.Items.add(it);")
		c.indent--
		c.writeln("}")
		c.writeln("return [for (var k in order) groups[k]!];")
		c.indent--
		c.writeln("}")
	}
	if c.useFetch {
		c.writeln("dynamic _fetch(String url, Map<String,dynamic>? opts) {")
		c.indent++
		c.writeln("var args = ['-s'];")
		c.writeln("var method = opts?['method']?.toString() ?? 'GET';")
		c.writeln("args.addAll(['-X', method]);")
		c.writeln("if (opts?['headers'] != null) {")
		c.indent++
		c.writeln("for (var e in (opts!['headers'] as Map).entries) {")
		c.indent++
		c.writeln("args.addAll(['-H', '${e.key}: ${e.value}']);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts?['query'] != null) {")
		c.indent++
		c.writeln("var qs = Uri(queryParameters: (opts!['query'] as Map).map((k,v)=>MapEntry(k.toString(), v.toString()))).query;")
		c.writeln("var sep = url.contains('?') ? '&' : '?';")
		c.writeln("url = url + sep + qs;")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts != null && opts.containsKey('body')) {")
		c.indent++
		c.writeln("args.addAll(['-d', jsonEncode(opts['body'])]);")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts?['timeout'] != null) {")
		c.indent++
		c.writeln("args.addAll(['--max-time', opts!['timeout'].toString()]);")
		c.indent--
		c.writeln("}")
		c.writeln("args.add(url);")
		c.writeln("var res = Process.runSync('curl', args);")
		c.writeln("return jsonDecode(res.stdout.toString());")
		c.indent--
		c.writeln("}")
	}
	if c.useLoad {
		c.writeln("List<Map<String,dynamic>> _load(String? path, Map<String,dynamic>? opts) {")
		c.indent++
		c.writeln("var format = (opts?['format'] ?? 'csv').toString();")
		c.writeln("var header = opts?['header'] ?? true;")
		c.writeln("var delim = (opts?['delimiter'] ?? ',').toString();")
		c.writeln("if (delim.isEmpty) delim = ',';")
		c.writeln("if (format == 'tsv') delim = '\t';")
		c.writeln("String text;")
		c.writeln("if (path == null || path == '' || path == '-') {")
		c.indent++
		c.writeln("var lines = <String>[];")
		c.writeln("while (true) {")
		c.indent++
		c.writeln("var line = stdin.readLineSync();")
		c.writeln("if (line == null) break;")
		c.writeln("lines.add(line);")
		c.indent--
		c.writeln("}")
		c.writeln("text = lines.join('\\n');")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("text = File(path).readAsStringSync();")
		c.indent--
		c.writeln("}")
		c.writeln("if (format != 'csv') return [];")
		c.writeln("var lines = text.trim().split(RegExp('\\r?\\n')).where((l) => l.isNotEmpty).toList();")
		c.writeln("if (lines.isEmpty) return [];")
		c.writeln("List<String> headers;")
		c.writeln("if (header) {")
		c.indent++
		c.writeln("headers = lines[0].split(delim);")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("headers = List.generate(lines[0].split(delim).length, (i) => 'c$' + i.toString());")
		c.indent--
		c.writeln("}")
		c.writeln("var start = header ? 1 : 0;")
		c.writeln("var out = <Map<String,dynamic>>[];")
		c.writeln("for (var i = start; i < lines.length; i++) {")
		c.indent++
		c.writeln("var parts = lines[i].split(delim);")
		c.writeln("var row = <String,dynamic>{};")
		c.writeln("for (var j = 0; j < headers.length; j++) {")
		c.indent++
		c.writeln("row[headers[j]] = j < parts.length ? parts[j] : '';")
		c.indent--
		c.writeln("}")
		c.writeln("out.add(row);")
		c.indent--
		c.writeln("}")
		c.writeln("return out;")
		c.indent--
		c.writeln("}")
	}
	if c.useSave {
		c.writeln("void _save(List<Map<String,dynamic>> rows, String? path, Map<String,dynamic>? opts) {")
		c.indent++
		c.writeln("var format = (opts?['format'] ?? 'csv').toString();")
		c.writeln("var header = opts?['header'] ?? false;")
		c.writeln("var delim = (opts?['delimiter'] ?? ',').toString();")
		c.writeln("if (delim.isEmpty) delim = ',';")
		c.writeln("if (format == 'tsv') delim = '\t';")
		c.writeln("if (format != 'csv') return;")
		c.writeln("var headers = rows.isNotEmpty ? (rows[0].keys.toList()..sort()) : <String>[];")
		c.writeln("var lines = <String>[];")
		c.writeln("if (header) lines.add(headers.join(delim));")
		c.writeln("for (var row in rows) {")
		c.indent++
		c.writeln("lines.add(headers.map((h) => row[h]?.toString() ?? '').join(delim));")
		c.indent--
		c.writeln("}")
		c.writeln("var text = lines.join('\\n') + '\n';")
		c.writeln("if (path == null || path == '' || path == '-') {")
		c.indent++
		c.writeln("stdout.write(text);")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("File(path).writeAsStringSync(text);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
	}
	if c.useQuery {
		c.writeln("List<dynamic> _query(List<dynamic> src, List<Map<String,dynamic>> joins, Map<String,dynamic> opts) {")
		c.indent++
		c.writeln("var items = [for (var v in src) [v]];")
		c.writeln("for (var j in joins) {")
		c.indent++
		c.writeln("var joined = <List<dynamic>>[];")
		c.writeln("var jitems = (j['items'] as List).cast<dynamic>();")
		c.writeln("var on = j['on'];")
		c.writeln("var left = j['left'] == true;")
		c.writeln("var right = j['right'] == true;")
		c.writeln("if (right && left) {")
		c.indent++
		c.writeln("var matched = List<bool>.filled(jitems.length, false);")
		c.writeln("for (var leftRow in items) {")
		c.indent++
		c.writeln("var m = false;")
		c.writeln("for (var ri = 0; ri < jitems.length; ri++) {")
		c.indent++
		c.writeln("var rightRow = jitems[ri];")
		c.writeln("var keep = true;")
		c.writeln("if (on != null) keep = Function.apply(on, [...leftRow, rightRow]) as bool;")
		c.writeln("if (!keep) continue;")
		c.writeln("m = true; matched[ri] = true;")
		c.writeln("joined.add([...leftRow, rightRow]);")
		c.indent--
		c.writeln("}")
		c.writeln("if (!m) joined.add([...leftRow, null]);")
		c.indent--
		c.writeln("}")
		c.writeln("for (var ri = 0; ri < jitems.length; ri++) {")
		c.indent++
		c.writeln("if (!matched[ri]) {")
		c.indent++
		c.writeln("var undef = items.isNotEmpty ? List<dynamic>.filled(items[0].length, null) : <dynamic>[];")
		c.writeln("joined.add([...undef, jitems[ri]]);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("} else if (right) {")
		c.indent++
		c.writeln("for (var rightRow in jitems) {")
		c.indent++
		c.writeln("var m = false;")
		c.writeln("for (var leftRow in items) {")
		c.indent++
		c.writeln("var keep = true;")
		c.writeln("if (on != null) keep = Function.apply(on, [...leftRow, rightRow]) as bool;")
		c.writeln("if (!keep) continue;")
		c.writeln("m = true; joined.add([...leftRow, rightRow]);")
		c.indent--
		c.writeln("}")
		c.writeln("if (!m) {")
		c.indent++
		c.writeln("var undef = items.isNotEmpty ? List<dynamic>.filled(items[0].length, null) : <dynamic>[];")
		c.writeln("joined.add([...undef, rightRow]);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("} else {")
		c.indent++
		c.writeln("for (var leftRow in items) {")
		c.indent++
		c.writeln("var m = false;")
		c.writeln("for (var rightRow in jitems) {")
		c.indent++
		c.writeln("var keep = true;")
		c.writeln("if (on != null) keep = Function.apply(on, [...leftRow, rightRow]) as bool;")
		c.writeln("if (!keep) continue;")
		c.writeln("m = true; joined.add([...leftRow, rightRow]);")
		c.indent--
		c.writeln("}")
		c.writeln("if (left && !m) joined.add([...leftRow, null]);")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("items = joined;")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts['where'] != null) {")
		c.indent++
		c.writeln("items = [for (var r in items) if (Function.apply(opts['where'], r) as bool) r];")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts['sortKey'] != null) {")
		c.indent++
		c.writeln("var pairs = [for (var it in items) {'item': it, 'key': Function.apply(opts['sortKey'], it)}];")
		c.writeln("pairs.sort((a,b) {")
		c.indent++
		c.writeln("var ak = a['key']; var bk = b['key'];")
		c.writeln("if (ak is num && bk is num) return ak.compareTo(bk);")
		c.writeln("if (ak is String && bk is String) return ak.compareTo(bk);")
		c.writeln("return ak.toString().compareTo(bk.toString());")
		c.indent--
		c.writeln("});")
		c.writeln("items = [for (var p in pairs) p['item'] as List<dynamic>];")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts['skip'] != null) {")
		c.indent++
		c.writeln("var n = opts['skip'] as int;")
		c.writeln("items = n < items.length ? items.sublist(n) : <List<dynamic>>[];")
		c.indent--
		c.writeln("}")
		c.writeln("if (opts['take'] != null) {")
		c.indent++
		c.writeln("var n = opts['take'] as int;")
		c.writeln("if (n < items.length) items = items.sublist(0, n);")
		c.indent--
		c.writeln("}")
		c.writeln("var res = <dynamic>[];")
		c.writeln("for (var r in items) { res.add(Function.apply(opts['select'], r)); }")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
	}
	if c.useGenText {
		c.writeln("String _genText(String prompt, String model, Map<String,dynamic>? params) {")
		c.indent++
		c.writeln("// TODO: integrate with an LLM")
		c.writeln("return prompt;")
		c.indent--
		c.writeln("}")
	}
	if c.useGenEmbed {
		c.writeln("List<double> _genEmbed(String text, String model, Map<String,dynamic>? params) {")
		c.indent++
		c.writeln("return text.codeUnits.map((c) => c.toDouble()).toList();")
		c.indent--
		c.writeln("}")
	}
	if c.useGenStruct {
		c.writeln("T _genStruct<T>(String prompt, String model, Map<String,dynamic>? params) {")
		c.indent++
		c.writeln("// TODO: parse model output into a struct")
		c.writeln("return null as T;")
		c.indent--
		c.writeln("}")
	}
}
