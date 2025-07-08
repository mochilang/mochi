//go:build slow

package dart

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler is a very small proof-of-concept translator that converts a limited
// subset of Mochi programs into Dart code.  For many of the test programs this
// translation is incomplete, but it is sufficient for simple examples such as
// variable declarations and basic arithmetic/print statements.
type Compiler struct {
	env       *types.Env
	buf       bytes.Buffer
	indent    int
	useIn     bool
	useJSON   bool
	tmp       int
	mapVars   map[string]bool
	groupKeys map[string]string
}

// New creates a new Dart compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, mapVars: make(map[string]bool), groupKeys: make(map[string]string)}
}

// Compile translates the given Mochi program into Dart source code.  If there
// is a hand written translation under tests/human/x/dart it is returned.
// Otherwise the program is compiled using the small subset supported here.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// compile function declarations first so that they appear before main
	c.buf.Reset()
	c.indent = 0
	c.useIn = false
	c.useJSON = false
	c.mapVars = make(map[string]bool)
	c.groupKeys = make(map[string]string)

	// compile function declarations into a separate buffer so they appear
	// before the main entry point
	var fnBuf bytes.Buffer
	old := c.buf
	c.buf.Reset()

	for _, st := range prog.Statements {
		switch {
		case st.Fun != nil:
			if err := c.compileFun(st.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		case st.Type != nil:
			if err := c.compileType(st.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	fnBuf.Write(c.buf.Bytes())
	c.buf.Reset()

	c.writeln("void main() {")
	c.indent++
	for _, st := range prog.Statements {
		if st.Fun != nil {
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")

	mainBytes := c.buf.Bytes()
	c.buf = old

	var out bytes.Buffer
	if c.useJSON {
		out.WriteString("import 'dart:convert';\n\n")
	}
	if c.useIn {
		out.WriteString("bool _in(dynamic item, dynamic col) {\n")
		out.WriteString("  if (col is Map) return col.containsKey(item);\n")
		out.WriteString("  if (col is Iterable || col is String) return col.contains(item);\n")
		out.WriteString("  return false;\n")
		out.WriteString("}\n\n")
	}

	out.Write(fnBuf.Bytes())
	out.Write(mainBytes)
	return out.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Type != nil:
		// type declarations are ignored in generated code
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Test != nil:
		// test blocks are ignored
		return nil
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Fun != nil:
		// handled earlier
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	typ := dartType(l.Type)
	var val string
	var err error
	if l.Value != nil {
		val, err = c.compileExpr(l.Value)
		if err != nil {
			return err
		}
	} else {
		val = defaultValue(typ)
	}
	if typ == "" {
		c.writeln(fmt.Sprintf("var %s = %s;", l.Name, val))
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, l.Name, val))
	}
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	typ := dartType(v.Type)
	var val string
	var err error
	if v.Value != nil {
		val, err = c.compileExpr(v.Value)
		if err != nil {
			return err
		}
	} else {
		val = defaultValue(typ)
	}
	if typ == "" {
		c.writeln(fmt.Sprintf("var %s = %s;", v.Name, val))
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", typ, v.Name, val))
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	target := a.Name
	for i, idx := range a.Index {
		if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
			return fmt.Errorf("slice assignment not supported")
		}
		if idx.Start == nil {
			return fmt.Errorf("missing index expression")
		}
		expr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		if i < len(a.Index)-1 || len(a.Field) > 0 {
			target = fmt.Sprintf("(%s[%s] as Map)", target, expr)
		} else {
			target += fmt.Sprintf("[%s]", expr)
		}
	}
	for _, f := range a.Field {
		target += "." + f.Name
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", target, val))
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	val, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + val + ";")
	return nil
}

func (c *Compiler) compileType(td *parser.TypeDecl) error {
	st, ok := c.env.GetStruct(td.Name)
	if !ok {
		return nil
	}
	c.writeln(fmt.Sprintf("class %s {", td.Name))
	c.indent++
	fields := make([]string, len(st.Order))
	for i, name := range st.Order {
		typ := dartTypeFromType(st.Fields[name])
		if typ == "" {
			typ = "dynamic"
		}
		c.writeln(fmt.Sprintf("%s %s;", typ, name))
		fields[i] = "this." + name
	}
	c.writeln(fmt.Sprintf("%s(%s);", td.Name, strings.Join(fields, ", ")))
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	ret := dartType(f.Return)
	if ret == "" {
		ret = "void"
	}
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		pt := dartType(p.Type)
		if pt == "" {
			pt = "dynamic"
		}
		params[i] = fmt.Sprintf("%s %s", pt, p.Name)
	}
	c.writeln(fmt.Sprintf("%s %s(%s) {", ret, f.Name, strings.Join(params, ", ")))
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
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
	c.writeln("}")
	if s.ElseIf != nil {
		c.writeln("else ")
		if err := c.compileIf(s.ElseIf); err != nil {
			return err
		}
	} else if s.Else != nil {
		c.writeln("else {")
		c.indent++
		for _, st := range s.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
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
		c.writeln(fmt.Sprintf("for (var %s = %s; %s < %s; %s++) {", f.Name, start, f.Name, end, f.Name))
	} else {
		srcExpr := f.Source
		src, err := c.compileExpr(srcExpr)
		if err != nil {
			return err
		}
		iterVar := fmt.Sprintf("_iter%d", c.tmp)
		c.tmp++
		c.writeln(fmt.Sprintf("var %s = %s;", iterVar, src))

		// mark loop variable as map-backed if the source is a list of maps
		t := types.TypeOfExpr(srcExpr, c.env)
		if lt, ok := t.(types.ListType); ok {
			if _, ok := lt.Elem.(types.MapType); ok {
				c.mapVars[f.Name] = true
			}
		}

		c.writeln(fmt.Sprintf("for (var %s in (%s is Map ? (%s as Map).keys : %s) as Iterable) {", f.Name, iterVar, iterVar, iterVar))
	}
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := u.Target
	idxVar := fmt.Sprintf("_i%d", c.tmp)
	c.tmp++
	itemVar := fmt.Sprintf("_it%d", c.tmp)
	c.tmp++

	c.writeln(fmt.Sprintf("for (var %s = 0; %s < %s.length; %s++) {", idxVar, idxVar, list, idxVar))
	c.indent++
	c.writeln(fmt.Sprintf("var %s = %s[%s];", itemVar, list, idxVar))

	var st types.StructType
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}

	origEnv := c.env
	if st.Name != "" {
		child := types.NewEnv(c.env)
		for _, f := range st.Order {
			c.writeln(fmt.Sprintf("var %s = %s.%s;", f, itemVar, f))
			child.SetVar(f, st.Fields[f], true)
		}
		c.env = child
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln(fmt.Sprintf("if (%s) {", cond))
		c.indent++
	}

	for _, it := range u.Set.Items {
		if st.Name != "" {
			if key, ok := c.simpleIdentifier(it.Key); ok {
				val, err := c.compileExpr(it.Value)
				if err != nil {
					c.env = origEnv
					return err
				}
				c.writeln(fmt.Sprintf("%s.%s = %s;", itemVar, key, val))
				continue
			}
		}
		keyExpr, err := c.compileExpr(it.Key)
		if err != nil {
			c.env = origEnv
			return err
		}
		valExpr, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln(fmt.Sprintf("%s[%s] = %s;", itemVar, keyExpr, valExpr))
	}

	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	c.writeln(fmt.Sprintf("%s[%s] = %s;", list, idxVar, itemVar))
	if st.Name != "" {
		c.env = origEnv
	}
	c.indent--
	c.writeln("}")
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
	leftType := types.TypeOfUnary(b.Left, c.env)
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightType := types.TypeOfPostfix(op.Right, c.env)
		cur := res
		switch op.Op {
		case "in":
			c.useIn = true
			res = fmt.Sprintf("_in(%s, %s)", res, r)
		case "union":
			if op.All {
				res = fmt.Sprintf("List.from(%s)..addAll(%s)", res, r)
			} else {
				res = fmt.Sprintf("{...%s, ...%s}.toList()", res, r)
			}
		case "except":
			res = fmt.Sprintf("List.from(%s)..removeWhere((x) => %s.contains(x))", res, r)
		case "intersect":
			res = fmt.Sprintf("%s.where((x) => %s.contains(x)).toList()", res, r)
		default:
			// string comparison
			if (op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=") &&
				strings.HasPrefix(cur, "'") && strings.HasPrefix(r, "'") {
				cmp := fmt.Sprintf("%s.compareTo(%s)", cur, r)
				switch op.Op {
				case "<":
					res = fmt.Sprintf("%s < 0", cmp)
				case "<=":
					res = fmt.Sprintf("%s <= 0", cmp)
				case ">":
					res = fmt.Sprintf("%s > 0", cmp)
				case ">=":
					res = fmt.Sprintf("%s >= 0", cmp)
				}
			} else {
				l := res
				rr := r
				if isNumericOp(op.Op) {
					if !isNumericType(leftType) {
						l = fmt.Sprintf("(%s as num)", l)
					}
					if !isNumericType(rightType) {
						rr = fmt.Sprintf("(%s as num)", rr)
					}
				}
				res = fmt.Sprintf("%s %s %s", l, op.Op, rr)
			}
		}
		leftType = types.ResultType(op.Op, leftType, rightType)
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	t := types.TypeOfPostfix(u.Value, c.env)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			if !isNumericType(t) {
				val = fmt.Sprintf("-(%s as num)", val)
				continue
			}
		}
		val = op + val
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
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.End != nil || op.Index.Step != nil {
				if op.Index.Step != nil || op.Index.Colon2 != nil {
					return "", fmt.Errorf("slices not supported")
				}
				start := "0"
				if op.Index.Start != nil {
					start, err = c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
				}
				end := fmt.Sprintf("%s.length", val)
				if op.Index.End != nil {
					end, err = c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
				}
				val = fmt.Sprintf("((%s is String) ? %s.substring(%s, %s) : (%s as List).sublist(%s, %s))", val, val, start, end, val, start, end)
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("missing index expression")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				val = fmt.Sprintf("%s[%s]", val, idx)
			}
		case op.Field != nil:
			val += fmt.Sprintf("['%s']", op.Field.Name)
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
		case op.Cast != nil:
			typ := dartType(op.Cast.Type)
			switch typ {
			case "int":
				val = fmt.Sprintf("int.parse(%s)", val)
			case "double":
				val = fmt.Sprintf("double.parse(%s)", val)
			case "String":
				val = fmt.Sprintf("%s.toString()", val)
			case "bool":
				val = fmt.Sprintf("(%s ? true : false)", val)
			default:
				if st, ok := c.env.GetStruct(typ); ok {
					args := make([]string, len(st.Order))
					for i, name := range st.Order {
						ftyp := dartTypeFromType(st.Fields[name])
						if ftyp == "" {
							ftyp = "dynamic"
						}
						args[i] = fmt.Sprintf("(%s['%s'] as %s)", val, name, ftyp)
					}
					val = fmt.Sprintf("%s(%s)", typ, strings.Join(args, ", "))
				} else {
					return "", fmt.Errorf("casts not supported")
				}
			}
		}
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
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, m := range p.Map.Items {
			k, err := c.compileMapKey(m.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(m.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s: %s", k, v)
		}
		return "{" + strings.Join(items, ", ") + "}", nil
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Selector != nil:
		return c.compileSelector(p.Selector), nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Group != nil:
		e, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + e + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) compileIfExpr(ix *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ix.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ix.Then)
	if err != nil {
		return "", err
	}
	if ix.ElseIf != nil {
		elseExpr, err := c.compileIfExpr(ix.ElseIf)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
	}
	elseCode := "null"
	if ix.Else != nil {
		elseCode, err = c.compileExpr(ix.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseCode), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	var b bytes.Buffer
	tmp := fmt.Sprintf("_q%d", c.tmp)
	c.tmp++

	w := func(s string) {
		b.WriteString(s)
	}

	w("(() {\n")
	w(fmt.Sprintf("  var %s = <dynamic>[];\n", tmp))
	grpVar := ""
	groups := ""
	keyVar := ""
	if q.Group != nil {
		groups = fmt.Sprintf("_g%d", c.tmp)
		c.tmp++
		w(fmt.Sprintf("  var %s = <dynamic, List<dynamic>>{};\n", groups))
	}

	loops := []string{fmt.Sprintf("var %s in %s", q.Var, c.mustExpr(q.Source))}
	// mark main variable type
	if t := types.TypeOfExpr(q.Source, c.env); t != nil {
		if lt, ok := t.(types.ListType); ok {
			if _, ok := lt.Elem.(types.MapType); ok {
				c.mapVars[q.Var] = true
			}
		}
	}
	for _, f := range q.Froms {
		loops = append(loops, fmt.Sprintf("var %s in %s", f.Var, c.mustExpr(f.Src)))
		if t := types.TypeOfExpr(f.Src, c.env); t != nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.MapType); ok {
					c.mapVars[f.Var] = true
				}
			}
		}
	}

	for i, loop := range loops {
		w(strings.Repeat("  ", i+1) + "for (" + loop + ") {\n")
	}

	if q.Where != nil {
		cond := c.mustExpr(q.Where)
		w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("if (!(%s)) continue;\n", cond))
	}

	sel := c.mustExpr(q.Select)
	if q.Group != nil {
		keyVar = fmt.Sprintf("_k%d", c.tmp)
		c.tmp++
		keyExpr := c.mustExpr(q.Group.Exprs[0])
		grpVar = q.Group.Name
		c.groupKeys[grpVar] = keyVar
		c.mapVars[grpVar] = true
		w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("var %s = %s;\n", keyVar, keyExpr))
		w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("%s.putIfAbsent(%s, () => <dynamic>[]).add(%s);\n", groups, keyVar, q.Var))
	} else {
		if q.Sort != nil {
			key := c.mustExpr(q.Sort)
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("%s.add([%s, %s]);\n", tmp, key, sel))
		} else {
			w(strings.Repeat("  ", len(loops)+1) + fmt.Sprintf("%s.add(%s);\n", tmp, sel))
		}
	}

	for i := len(loops) - 1; i >= 0; i-- {
		w(strings.Repeat("  ", i+1) + "}\n")
	}

	if q.Group != nil {
		w(fmt.Sprintf("  for (var entry in %s.entries) {\n", groups))
		w(fmt.Sprintf("    var %s = entry.value;\n", grpVar))
		w(fmt.Sprintf("    var %s = entry.key;\n", keyVar))
		if q.Group.Having != nil {
			cond := c.mustExpr(q.Group.Having)
			w(fmt.Sprintf("    if (!(%s)) continue;\n", cond))
		}
		sel := c.mustExpr(q.Select)
		if q.Sort != nil {
			key := c.mustExpr(q.Sort)
			w(fmt.Sprintf("    %s.add([%s, %s]);\n", tmp, key, sel))
		} else {
			w(fmt.Sprintf("    %s.add(%s);\n", tmp, sel))
		}
		w("  }\n")
	}

	if q.Sort != nil {
		w(fmt.Sprintf("  %s.sort((a,b) => (a[0] as Comparable).compareTo(b[0]));\n", tmp))
		w(fmt.Sprintf("  %s = [for (var x in %s) x[1]];\n", tmp, tmp))
	}
	if q.Skip != nil || q.Take != nil {
		start := "0"
		if q.Skip != nil {
			start = c.mustExpr(q.Skip)
		}
		end := fmt.Sprintf("%s.length", tmp)
		if q.Take != nil {
			end = c.mustExpr(q.Take)
			if q.Skip != nil {
				end = fmt.Sprintf("(%s)+(%s)", start, end)
			}
		}
		w(fmt.Sprintf("  %s = %s.sublist(%s, %s);\n", tmp, tmp, start, end))
	}
	if q.Distinct {
		w(fmt.Sprintf("  %s = %s.toSet().toList();\n", tmp, tmp))
	}
	w(fmt.Sprintf("  return %s;\n", tmp))
	w("})()")
	return b.String(), nil
}

func (c *Compiler) compileStructLiteral(sl *parser.StructLiteral) (string, error) {
	fieldMap := make(map[string]string)
	for _, f := range sl.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fieldMap[f.Name] = v
	}
	if st, ok := c.env.GetStruct(sl.Name); ok {
		args := make([]string, len(st.Order))
		for i, name := range st.Order {
			if v, ok := fieldMap[name]; ok {
				args[i] = v
			} else {
				args[i] = defaultValue(dartTypeFromType(st.Fields[name]))
			}
		}
		return fmt.Sprintf("%s(%s)", sl.Name, strings.Join(args, ", ")), nil
	}
	items := make([]string, 0, len(sl.Fields))
	for name, v := range fieldMap {
		items = append(items, fmt.Sprintf("'%s': %s", name, v))
	}
	return "{" + strings.Join(items, ", ") + "}", nil
}

func (c *Compiler) mustExpr(e *parser.Expr) string {
	s, err := c.compileExpr(e)
	if err != nil {
		return "null"
	}
	return s
}

func (c *Compiler) compileMapKey(e *parser.Expr) (string, error) {
	if name, ok := c.simpleIdentifier(e); ok {
		return fmt.Sprintf("'%s'", name), nil
	}
	return c.compileExpr(e)
}

func (c *Compiler) simpleIdentifier(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) > 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func (c *Compiler) compileSelector(sel *parser.SelectorExpr) string {
	root := sel.Root
	if key, ok := c.groupKeys[root]; ok {
		if len(sel.Tail) == 1 && sel.Tail[0] == "key" {
			return key
		}
	}
	typ, _ := c.env.GetVar(root)
	if ok := c.mapVars[root]; ok {
		typ = types.MapType{}
	}
	if _, ok := typ.(types.MapType); ok {
		s := root
		for _, part := range sel.Tail {
			s += fmt.Sprintf("['%s']", part)
		}
		return s
	}
	s := root
	if len(sel.Tail) > 0 {
		s += "." + strings.Join(sel.Tail, ".")
	}
	return s
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	// handle simple builtins so the generated Dart code is runnable without
	// additional support libraries
	switch call.Func {
	case "print":
		if len(args) == 1 {
			return fmt.Sprintf("print(%s)", args[0]), nil
		}
		return fmt.Sprintf("print([%s].join(' '))", strings.Join(args, ", ")), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("List.from(%s)..add(%s)", args[0], args[1]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		a := args[0]
		return fmt.Sprintf("(%s.isEmpty ? 0 : %s.reduce((a, b) => a + b) / %s.length)", a, a, a), nil
	case "count", "len":
		if len(args) != 1 {
			return "", fmt.Errorf("%s expects 1 arg", call.Func)
		}
		return fmt.Sprintf("%s.length", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		a := args[0]
		return fmt.Sprintf("%s.reduce((a, b) => a + b)", a), nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		a := args[0]
		return fmt.Sprintf("%s.reduce((a, b) => a < b ? a : b)", a), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		a := args[0]
		return fmt.Sprintf("%s.reduce((a, b) => a > b ? a : b)", a), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("%s.toString()", args[0]), nil
	case "substring":
		if len(args) == 2 {
			return fmt.Sprintf("%s.substring(%s)", args[0], args[1]), nil
		} else if len(args) == 3 {
			return fmt.Sprintf("%s.substring(%s, %s)", args[0], args[1], args[2]), nil
		}
		return "", fmt.Errorf("substring expects 2 or 3 args")
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("%s.values.toList()", args[0]), nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		c.useJSON = true
		return fmt.Sprintf("print(jsonEncode(%s))", args[0]), nil
	}

	return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
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
		s := strings.ReplaceAll(*l.Str, "'", "\\'")
		s = strings.ReplaceAll(s, "$", "\\$")
		return "'" + strings.Trim(s, "\"") + "'"
	case l.Null:
		return "null"
	default:
		return "null"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func dartTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "bool"
	case types.ListType:
		elem := dartTypeFromType(tt.Elem)
		if elem == "" {
			elem = "dynamic"
		}
		return fmt.Sprintf("List<%s>", elem)
	case types.MapType:
		k := dartTypeFromType(tt.Key)
		if k == "" {
			k = "dynamic"
		}
		v := dartTypeFromType(tt.Value)
		if v == "" {
			v = "dynamic"
		}
		return fmt.Sprintf("Map<%s, %s>", k, v)
	case types.StructType:
		return tt.Name
	default:
		return ""
	}
}

func dartType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Fun != nil {
		return "Function"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "double"
		case "string":
			return "String"
		case "bool":
			return "bool"
		}
		return *t.Simple
	}
	if t.Generic != nil {
		name := t.Generic.Name
		if name == "list" && len(t.Generic.Args) == 1 {
			elem := dartType(t.Generic.Args[0])
			if elem == "" {
				elem = "dynamic"
			}
			return fmt.Sprintf("List<%s>", elem)
		}
		if name == "map" && len(t.Generic.Args) == 2 {
			k := dartType(t.Generic.Args[0])
			if k == "" {
				k = "dynamic"
			}
			v := dartType(t.Generic.Args[1])
			if v == "" {
				v = "dynamic"
			}
			return fmt.Sprintf("Map<%s, %s>", k, v)
		}
		return name
	}
	return ""
}

func defaultValue(typ string) string {
	switch typ {
	case "int":
		return "0"
	case "double":
		return "0.0"
	case "String":
		return "''"
	case "bool":
		return "false"
	default:
		if strings.HasPrefix(typ, "List<") {
			return "[]"
		}
		if strings.HasPrefix(typ, "Map<") {
			return "{}"
		}
		return "null"
	}
}

func isNumericType(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.FloatType:
		return true
	default:
		return false
	}
}

func isNumericOp(op string) bool {
	switch op {
	case "+", "-", "*", "/", "%", "<", "<=", ">", ">=":
		return true
	default:
		return false
	}
}

func findRepoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}
