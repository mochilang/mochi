//go:build slow

package swift

import (
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compile parses Mochi source and generates Swift code. Only a subset of
// Mochi is supported. Unsupported features result in an error.
func Compile(src string) ([]byte, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	c := New(nil)
	return c.Compile(prog)
}

// Compiler converts a subset of Mochi into Swift source code.
type Compiler struct {
	compiler
}

// New creates a new Compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{compiler: compiler{
		structs:  make(map[string][]string),
		inout:    make(map[string][]bool),
		varTypes: make(map[string]string),
	}}
}

// Compile generates Swift code for the given program.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	if err := c.program(p); err != nil {
		return nil, err
	}
	return []byte(c.buf.String()), nil
}

type compiler struct {
	buf      strings.Builder
	indent   int
	structs  map[string][]string
	inout    map[string][]bool
	varTypes map[string]string
	tupleMap bool
}

func (c *compiler) program(p *parser.Program) error {
	for _, s := range p.Statements {
		if err := c.stmt(s); err != nil {
			return err
		}
	}
	return nil
}

func (c *compiler) stmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.letStmt(s.Let)
	case s.Var != nil:
		return c.varStmt(s.Var)
	case s.Assign != nil:
		return c.assignStmt(s.Assign)
	case s.Fun != nil:
		return c.funStmt(s.Fun)
	case s.Type != nil:
		return c.typeDecl(s.Type)
	case s.Return != nil:
		return c.returnStmt(s.Return)
	case s.If != nil:
		return c.ifStmt(s.If)
	case s.While != nil:
		return c.whileStmt(s.While)
	case s.For != nil:
		return c.forStmt(s.For)
	case s.Break != nil:
		return c.breakStmt(s.Break)
	case s.Continue != nil:
		return c.continueStmt(s.Continue)
	case s.Expr != nil:
		expr, err := c.expr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *compiler) letStmt(l *parser.LetStmt) error {
	typ, err := c.typeRef(l.Type)
	if err != nil {
		return err
	}
	if l.Value != nil {
		val, err := c.expr(l.Value)
		if err != nil {
			return err
		}
		if typ != "" {
			c.writeln(fmt.Sprintf("let %s: %s = %s", l.Name, typ, val))
		} else {
			c.writeln(fmt.Sprintf("let %s = %s", l.Name, val))
		}
		c.varTypes[l.Name] = c.inferType(l.Type, l.Value)
		return nil
	}
	if typ == "" {
		return fmt.Errorf("let without value or type at line %d", l.Pos.Line)
	}
	c.writeln(fmt.Sprintf("let %s: %s = %s", l.Name, typ, defaultValue(typ)))
	c.varTypes[l.Name] = c.inferType(l.Type, nil)
	return nil
}

func (c *compiler) varStmt(v *parser.VarStmt) error {
	typ, err := c.typeRef(v.Type)
	if err != nil {
		return err
	}
	if v.Value != nil {
		val, err := c.expr(v.Value)
		if err != nil {
			return err
		}
		if typ != "" {
			c.writeln(fmt.Sprintf("var %s: %s = %s", v.Name, typ, val))
		} else {
			c.writeln(fmt.Sprintf("var %s = %s", v.Name, val))
		}
		c.varTypes[v.Name] = c.inferType(v.Type, v.Value)
		return nil
	}
	if typ == "" {
		return fmt.Errorf("var without value or type at line %d", v.Pos.Line)
	}
	c.writeln(fmt.Sprintf("var %s: %s = %s", v.Name, typ, defaultValue(typ)))
	c.varTypes[v.Name] = c.inferType(v.Type, nil)
	return nil
}

func (c *compiler) assignStmt(a *parser.AssignStmt) error {
	lhs := a.Name
	for _, idx := range a.Index {
		if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil || idx.Start == nil {
			return fmt.Errorf("complex assignment not supported")
		}
		expr, err := c.expr(idx.Start)
		if err != nil {
			return err
		}
		lhs += fmt.Sprintf("[%s]", expr)
	}
	for _, f := range a.Field {
		lhs += "." + f.Name
	}

	val, err := c.expr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, val))
	if len(a.Index) == 0 && len(a.Field) == 0 {
		c.varTypes[a.Name] = c.inferType(nil, a.Value)
	}
	return nil
}

func (c *compiler) typeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return fmt.Errorf("variant types not supported")
	}
	c.structs[t.Name] = []string{}
	c.writeln(fmt.Sprintf("struct %s {", t.Name))
	c.indent++
	for _, m := range t.Members {
		if m.Field == nil || m.Method != nil {
			return fmt.Errorf("unsupported type member at line %d", t.Pos.Line)
		}
		typ, err := c.typeRef(m.Field.Type)
		if err != nil {
			return err
		}
		c.structs[t.Name] = append(c.structs[t.Name], m.Field.Name)
		c.writeln(fmt.Sprintf("var %s: %s", m.Field.Name, typ))
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) funStmt(f *parser.FunStmt) error {
	c.writeIndent()
	c.buf.WriteString("func ")
	c.buf.WriteString(f.Name)
	c.buf.WriteString("(")
	c.inout[f.Name] = make([]bool, len(f.Params))
	for i, p := range f.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		typ, err := c.typeRef(p.Type)
		if err != nil {
			return err
		}
		if typ == "" {
			return fmt.Errorf("parameter %s missing type", p.Name)
		}
		c.buf.WriteString("_ ")
		c.buf.WriteString(p.Name)
		c.buf.WriteString(": ")
		if !isBuiltinType(typ) {
			c.buf.WriteString("inout ")
			c.inout[f.Name][i] = true
		}
		c.buf.WriteString(typ)
	}
	c.buf.WriteString(")")
	if f.Return != nil {
		rtyp, err := c.typeRef(f.Return)
		if err != nil {
			return err
		}
		if rtyp != "" {
			c.buf.WriteString(" -> ")
			c.buf.WriteString(rtyp)
		}
	}
	c.buf.WriteString(" {\n")
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) returnStmt(r *parser.ReturnStmt) error {
	val, err := c.expr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + val)
	return nil
}

func (c *compiler) ifStmt(i *parser.IfStmt) error {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if " + cond + " {")
	c.indent++
	for _, st := range i.Then {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	if i.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("else ")
		if err := c.ifStmt(i.ElseIf); err != nil {
			return err
		}
		return nil
	}
	if len(i.Else) > 0 {
		c.writeln("else {")
		c.indent++
		for _, st := range i.Else {
			if err := c.stmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *compiler) whileStmt(w *parser.WhileStmt) error {
	cond, err := c.expr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " {")
	c.indent++
	for _, st := range w.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) forStmt(f *parser.ForStmt) error {
	var header string
	if f.RangeEnd != nil {
		start, err := c.expr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.expr(f.RangeEnd)
		if err != nil {
			return err
		}
		header = fmt.Sprintf("for %s in %s..<%s {", f.Name, start, end)
	} else {
		src, err := c.expr(f.Source)
		if err != nil {
			return err
		}
		header = fmt.Sprintf("for %s in %s {", f.Name, src)
	}
	c.writeln(header)
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) breakStmt(_ *parser.BreakStmt) error {
	c.writeln("break")
	return nil
}

func (c *compiler) continueStmt(_ *parser.ContinueStmt) error {
	c.writeln("continue")
	return nil
}

func (c *compiler) expr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expression")
	}
	return c.binary(e.Binary)
}

func (c *compiler) binary(b *parser.BinaryExpr) (string, error) {
	left, err := c.unary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	for _, op := range b.Right {
		if !supportedOp(op.Op) {
			return "", fmt.Errorf("unsupported operator %s at line %d", op.Op, op.Pos.Line)
		}
		r, err := c.postfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "in":
			typ := c.primaryType(op.Right.Target)
			if typ == "map" {
				res = fmt.Sprintf("%s.keys.contains(%s)", r, res)
			} else {
				res = fmt.Sprintf("%s.contains(%s)", r, res)
			}
		case "union":
			if op.All {
				res = fmt.Sprintf("(%s + %s)", res, r)
			} else {
				res = fmt.Sprintf("Array(Set(%s).union(%s)).sorted()", res, r)
			}
		case "except":
			res = fmt.Sprintf("Array(Set(%s).subtracting(%s)).sorted()", res, r)
		case "intersect":
			res = fmt.Sprintf("Array(Set(%s).intersection(%s)).sorted()", res, r)
		default:
			res = fmt.Sprintf("%s %s %s", res, op.Op, r)
		}
	}
	return res, nil
}

func supportedOp(op string) bool {
	switch op {
	case "+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "||", "in", "union", "except", "intersect":
		return true
	}
	return false
}

func (c *compiler) unary(u *parser.Unary) (string, error) {
	val, err := c.postfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op != "-" && op != "!" {
			return "", fmt.Errorf("unsupported unary operator %s at line %d", op, u.Pos.Line)
		}
		val = op + val
	}
	return val, nil
}

func (c *compiler) postfix(p *parser.PostfixExpr) (string, error) {
	// special case: map literal cast to struct
	if len(p.Ops) == 1 && p.Ops[0].Cast != nil && p.Target.Map != nil {
		typ, err := c.typeRef(p.Ops[0].Cast.Type)
		if err != nil {
			return "", err
		}
		if _, ok := c.structs[typ]; ok {
			return c.mapToStruct(p.Target.Map, typ)
		}
	}

	val, err := c.primary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			idx := op.Index
			if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
				start := "0"
				end := fmt.Sprintf("%s.count", val)
				if idx.Start != nil {
					start, err = c.expr(idx.Start)
					if err != nil {
						return "", err
					}
				}
				if idx.End != nil {
					end, err = c.expr(idx.End)
					if err != nil {
						return "", err
					}
				}
				typ := c.primaryType(p.Target)
				if typ == "string" {
					startIdx := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", val, val, start)
					endIdx := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", val, val, end)
					val = fmt.Sprintf("String(%s[%s..<%s])", val, startIdx, endIdx)
				} else {
					val = fmt.Sprintf("Array(%s[%s..<%s])", val, start, end)
				}
			} else {
				if idx.Start == nil {
					return "", fmt.Errorf("empty index")
				}
				s, err := c.expr(idx.Start)
				if err != nil {
					return "", err
				}
				typ := c.primaryType(p.Target)
				if typ == "string" {
					pos := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", val, val, s)
					val = fmt.Sprintf("%s[%s]", val, pos)
				} else {
					val = fmt.Sprintf("%s[%s]", val, s)
				}
			}
		case op.Field != nil:
			val = fmt.Sprintf("%s.%s", val, op.Field.Name)
		case op.Call != nil:
			parts := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.expr(a)
				if err != nil {
					return "", err
				}
				parts[i] = s
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(parts, ", "))
		case op.Cast != nil:
			typ, err := c.typeRef(op.Cast.Type)
			if err != nil {
				return "", err
			}
			switch typ {
			case "Int", "Double", "Bool", "String":
				val = fmt.Sprintf("%s(%s)", typ, val)
			default:
				return "", fmt.Errorf("unsupported cast to %s", typ)
			}
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return val, nil
}

func (c *compiler) primary(p *parser.Primary) (string, error) {
	switch {
	case p.FunExpr != nil:
		return c.funExpr(p.FunExpr)
	case p.Lit != nil:
		return literal(p.Lit), nil
	case p.Call != nil:
		return c.callExpr(p.Call)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.expr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		return c.mapLiteral(p.Map)
	case p.Struct != nil:
		return c.structLiteral(p.Struct)
	case p.If != nil:
		return c.ifExpr(p.If)
	case p.Match != nil:
		return c.matchExpr(p.Match)
	case p.Query != nil:
		return c.queryExpr(p.Query)
	case p.Selector != nil:
		return selector(p.Selector), nil
	case p.Group != nil:
		e, err := c.expr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + e + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *compiler) callExpr(call *parser.CallExpr) (string, error) {
	// special case: exists(query ...)
	if call.Func == "exists" && len(call.Args) == 1 {
		if q := queryArg(call.Args[0]); q != nil {
			return c.existsQuery(q)
		}
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.expr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	joined := strings.Join(args, ", ")
	switch call.Func {
	case "print":
		return fmt.Sprintf("print(%s)", joined), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 argument at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("String(%s)", joined), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 arguments at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("%s + [%s]", args[0], args[1]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 argument at line %d", call.Pos.Line)
		}
		a := args[0]
		return fmt.Sprintf("(%s.reduce(0, +) / %s.count)", a, a), nil
	case "count", "len":
		if len(args) != 1 {
			return "", fmt.Errorf("%s expects 1 argument at line %d", call.Func, call.Pos.Line)
		}
		return fmt.Sprintf("%s.count", args[0]), nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 argument at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("%s.min()!", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 argument at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("%s.max()!", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 argument at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("%s.reduce(0, +)", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 argument at line %d", call.Pos.Line)
		}
		if q := queryArg(call.Args[0]); q != nil {
			return c.existsQuery(q)
		}
		return fmt.Sprintf("!%s.isEmpty", args[0]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 argument at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("Array(%s.values)", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 arguments at line %d", call.Pos.Line)
		}
		s := args[0]
		start := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", s, s, args[1])
		end := fmt.Sprintf("%s.index(%s.startIndex, offsetBy: %s)", s, s, args[2])
		return fmt.Sprintf("String(%s[%s..<%s])", s, start, end), nil
	default:
		if flags, ok := c.inout[call.Func]; ok {
			for i := range flags {
				if flags[i] && i < len(args) {
					args[i] = "&" + args[i]
				}
			}
			joined = strings.Join(args, ", ")
		}
		return fmt.Sprintf("%s(%s)", call.Func, joined), nil
	}
}

func (c *compiler) funExpr(f *parser.FunExpr) (string, error) {
	var b strings.Builder
	b.WriteString("{ ")
	if len(f.Params) > 0 {
		b.WriteString("(")
		for i, p := range f.Params {
			if i > 0 {
				b.WriteString(", ")
			}
			typ, err := c.typeRef(p.Type)
			if err != nil {
				return "", err
			}
			b.WriteString(p.Name)
			if typ != "" {
				b.WriteString(": ")
				b.WriteString(typ)
			}
		}
		b.WriteString(") ")
	}
	if f.Return != nil {
		rt, err := c.typeRef(f.Return)
		if err != nil {
			return "", err
		}
		if rt != "" {
			b.WriteString("-> ")
			b.WriteString(rt)
			b.WriteString(" ")
		}
	}
	b.WriteString("in ")
	if f.ExprBody == nil {
		return "", fmt.Errorf("block closures not supported")
	}
	expr, err := c.expr(f.ExprBody)
	if err != nil {
		return "", err
	}
	b.WriteString(expr)
	b.WriteString(" }")
	return b.String(), nil
}

func (c *compiler) ifExpr(i *parser.IfExpr) (string, error) {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return "", err
	}
	thenVal, err := c.expr(i.Then)
	if err != nil {
		return "", err
	}
	var elseVal string
	if i.ElseIf != nil {
		elseVal, err = c.ifExpr(i.ElseIf)
		if err != nil {
			return "", err
		}
	} else if i.Else != nil {
		elseVal, err = c.expr(i.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("%s ? %s : %s", cond, thenVal, elseVal), nil
}

func (c *compiler) structLiteral(s *parser.StructLiteral) (string, error) {
	fields := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		v, err := c.expr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", f.Name, v)
	}
	return fmt.Sprintf("%s(%s)", s.Name, strings.Join(fields, ", ")), nil
}

func (c *compiler) matchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.expr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("{ () in\n")
	b.WriteString("    switch ")
	b.WriteString(target)
	b.WriteString(" {\n")
	for _, cse := range m.Cases {
		pat, err := c.expr(cse.Pattern)
		if err != nil {
			return "", err
		}
		if isWildcard(cse.Pattern) {
			b.WriteString("    default: return ")
		} else {
			b.WriteString("    case ")
			b.WriteString(pat)
			b.WriteString(": return ")
		}
		res, err := c.expr(cse.Result)
		if err != nil {
			return "", err
		}
		b.WriteString(res)
		b.WriteString("\n")
	}
	b.WriteString("    }\n")
	b.WriteString("}()")
	return b.String(), nil
}

func (c *compiler) mapToStruct(m *parser.MapLiteral, name string) (string, error) {
	fields := make([]string, len(m.Items))
	for i, it := range m.Items {
		key, ok := stringLiteral(it.Key)
		if !ok {
			return "", fmt.Errorf("struct field must be string literal")
		}
		v, err := c.expr(it.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", key, v)
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(fields, ", ")), nil
}

func (c *compiler) mapLiteral(m *parser.MapLiteral) (string, error) {
	if c.tupleMap {
		fields := make([]string, len(m.Items))
		for i, it := range m.Items {
			key, ok := keyName(it.Key)
			if !ok {
				c.tupleMap = false
				return c.mapLiteral(m)
			}
			v, err := c.expr(it.Value)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf("%s: %s", key, v)
		}
		return "(" + strings.Join(fields, ", ") + ")", nil
	}
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		var kstr string
		if name, ok := keyName(it.Key); ok {
			kstr = fmt.Sprintf("%q", name)
		} else {
			var err error
			kstr, err = c.expr(it.Key)
			if err != nil {
				return "", err
			}
		}
		v, err := c.expr(it.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("%s: %s", kstr, v)
	}
	return "[" + strings.Join(items, ", ") + "]", nil
}

func keyName(e *parser.Expr) (string, bool) {
	if s, ok := stringLiteral(e); ok {
		return s, true
	}
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return "", false
	}
	sel := e.Binary.Left.Value.Target.Selector
	if sel != nil && len(sel.Tail) == 0 {
		return sel.Root, true
	}
	return "", false
}

func stringLiteral(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return "", false
	}
	p := e.Binary.Left.Value.Target
	if p.Lit != nil && p.Lit.Str != nil {
		return *p.Lit.Str, true
	}
	return "", false
}

func isWildcard(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	sel := e.Binary.Left.Value.Target.Selector
	return sel != nil && sel.Root == "_" && len(sel.Tail) == 0
}

func selector(s *parser.SelectorExpr) string {
	if len(s.Tail) == 0 {
		return s.Root
	}
	return s.Root + "." + strings.Join(s.Tail, ".")
}

func literal(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if *l.Bool {
			return "true"
		}
		return "false"
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	default:
		return "nil"
	}
}

func (c *compiler) typeRef(t *parser.TypeRef) (string, error) {
	if t == nil {
		return "", nil
	}
	if t.Generic != nil {
		name := strings.ToLower(t.Generic.Name)
		switch name {
		case "list":
			if len(t.Generic.Args) != 1 {
				return "", fmt.Errorf("list requires 1 type arg")
			}
			et, err := c.typeRef(t.Generic.Args[0])
			if err != nil {
				return "", err
			}
			if et == "" {
				return "", fmt.Errorf("unsupported list element type")
			}
			return "[" + et + "]", nil
		case "map":
			if len(t.Generic.Args) != 2 {
				return "", fmt.Errorf("map requires 2 type args")
			}
			kt, err := c.typeRef(t.Generic.Args[0])
			if err != nil {
				return "", err
			}
			vt, err := c.typeRef(t.Generic.Args[1])
			if err != nil {
				return "", err
			}
			if kt == "" || vt == "" {
				return "", fmt.Errorf("unsupported map type")
			}
			return "[" + kt + ": " + vt + "]", nil
		}
		return "", fmt.Errorf("unsupported generic type %s", t.Generic.Name)
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			pt, err := c.typeRef(p)
			if err != nil {
				return "", err
			}
			if pt == "" {
				return "", fmt.Errorf("unsupported func param type")
			}
			params[i] = pt
		}
		ret := "Void"
		if t.Fun.Return != nil {
			rt, err := c.typeRef(t.Fun.Return)
			if err != nil {
				return "", err
			}
			if rt != "" {
				ret = rt
			}
		}
		return "(" + strings.Join(params, ", ") + ") -> " + ret, nil
	}
	if t.Simple == nil {
		return "", fmt.Errorf("unsupported type reference")
	}
	switch strings.ToLower(*t.Simple) {
	case "int":
		return "Int", nil
	case "string":
		return "String", nil
	case "float":
		return "Double", nil
	case "bool":
		return "Bool", nil
	default:
		return *t.Simple, nil
	}
}

func defaultValue(typ string) string {
	switch typ {
	case "Int":
		return "0"
	case "Double":
		return "0.0"
	case "Bool":
		return "false"
	case "String":
		return "\"\""
	default:
		return "nil"
	}
}

func isBuiltinType(typ string) bool {
	switch typ {
	case "Int", "Double", "Bool", "String":
		return true
	}
	if strings.HasPrefix(typ, "[") && strings.HasSuffix(typ, "]") {
		return true
	}
	return false
}

func (c *compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *compiler) inferType(t *parser.TypeRef, val *parser.Expr) string {
	if t != nil && t.Simple != nil {
		switch strings.ToLower(*t.Simple) {
		case "string":
			return "string"
		case "int", "float", "bool":
			return "number"
		}
	}
	if val != nil && val.Binary != nil && val.Binary.Left != nil {
		p := val.Binary.Left.Value
		if len(val.Binary.Right) == 0 && len(p.Ops) == 0 {
			switch {
			case p.Target.Lit != nil && p.Target.Lit.Str != nil:
				return "string"
			case p.Target.List != nil:
				return "list"
			case p.Target.Map != nil:
				return "map"
			}
		}
	}
	return ""
}

func (c *compiler) exprType(e *parser.Expr) string {
	if e == nil || e.Binary == nil {
		return ""
	}
	u := e.Binary.Left
	if u == nil {
		return ""
	}
	p := u.Value
	if p == nil || p.Target == nil {
		return ""
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if typ, ok := c.varTypes[p.Target.Selector.Root]; ok {
			return typ
		}
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return "string"
	}
	if p.Target.List != nil {
		return "list"
	}
	if p.Target.Map != nil {
		return "map"
	}
	return ""
}

func (c *compiler) primaryType(p *parser.Primary) string {
	if p == nil {
		return ""
	}
	switch {
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if typ, ok := c.varTypes[p.Selector.Root]; ok {
			return typ
		}
	case p.Lit != nil && p.Lit.Str != nil:
		return "string"
	case p.List != nil:
		return "list"
	case p.Map != nil:
		return "map"
	}
	return ""
}

func queryArg(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return nil
	}
	return e.Binary.Left.Value.Target.Query
}

func (c *compiler) existsQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return "", fmt.Errorf("unsupported query")
	}
	src, err := c.expr(q.Source)
	if err != nil {
		return "", err
	}
	if q.Select != nil && !isVarRef(q.Select, q.Var) {
		return "", fmt.Errorf("unsupported query")
	}
	if q.Where == nil {
		return fmt.Sprintf("!%s.isEmpty", src), nil
	}
	saved := c.varTypes
	c.varTypes = copyMap(c.varTypes)
	c.varTypes[q.Var] = c.exprType(q.Source)
	cond, err := c.expr(q.Where)
	c.varTypes = saved
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%s.contains { %s in %s }", src, q.Var, cond), nil
}

func (c *compiler) queryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return "", fmt.Errorf("unsupported query")
	}
	vars := []string{q.Var}
	srcs := []*parser.Expr{q.Source}
	for _, f := range q.Froms {
		vars = append(vars, f.Var)
		srcs = append(srcs, f.Src)
	}
	saved := c.varTypes
	c.varTypes = copyMap(c.varTypes)
	defer func() { c.varTypes = saved }()

	var build func(int) (string, error)
	build = func(i int) (string, error) {
		src, err := c.expr(srcs[i])
		if err != nil {
			return "", err
		}
		name := vars[i]
		if i == len(vars)-1 {
			prev := c.tupleMap
			c.tupleMap = true
			sel, err := c.expr(q.Select)
			c.tupleMap = prev
			if err != nil {
				return "", err
			}
			if q.Where != nil {
				cond, err := c.expr(q.Where)
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("%s.compactMap { %s in %s ? (%s) : nil }", src, name, cond, sel), nil
			}
			return fmt.Sprintf("%s.map { %s in %s }", src, name, sel), nil
		}
		inner, err := build(i + 1)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s.flatMap { %s in %s }", src, name, inner), nil
	}

	return build(0)
}

func isVarRef(e *parser.Expr, name string) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	sel := e.Binary.Left.Value.Target.Selector
	return sel != nil && sel.Root == name && len(sel.Tail) == 0
}

func copyMap[K comparable, V any](m map[K]V) map[K]V {
	out := make(map[K]V, len(m))
	for k, v := range m {
		out[k] = v
	}
	return out
}
