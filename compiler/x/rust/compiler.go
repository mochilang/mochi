//go:build slow

package rustcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a subset of Mochi to Rust source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	helpers map[string]bool
	tmp     int
	env     *types.Env
	structs map[string]types.StructType
}

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("tmp%d", c.tmp)
}

// New returns a new Compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{helpers: make(map[string]bool), env: env, structs: make(map[string]types.StructType)}
}

// Compile converts a parsed Mochi program into Rust source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.helpers = map[string]bool{}
	c.writeln("fn main() {")
	c.indent++
	for _, s := range prog.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")

	// prepend helpers
	var out bytes.Buffer
	if c.helpers["append"] {
		out.WriteString("fn append<T: Clone>(mut v: Vec<T>, item: T) -> Vec<T> {\n")
		out.WriteString("    v.push(item);\n    v\n}\n\n")
	}
	if c.helpers["avg"] {
		out.WriteString("fn avg(v: Vec<i32>) -> f64 {\n    let sum: i32 = v.iter().sum();\n    sum as f64 / v.len() as f64\n}\n\n")
	}
	if c.helpers["sum"] {
		out.WriteString("fn sum(v: Vec<i32>) -> i32 {\n    v.iter().sum()\n}\n\n")
	}
	if c.helpers["min"] {
		out.WriteString("fn min(v: Vec<i32>) -> i32 {\n    *v.iter().min().unwrap()\n}\n\n")
	}
	if c.helpers["max"] {
		out.WriteString("fn max(v: Vec<i32>) -> i32 {\n    *v.iter().max().unwrap()\n}\n\n")
	}
	if c.helpers["_union"] {
		out.WriteString("fn _union<T: Eq + std::hash::Hash + Clone>(a: Vec<T>, b: Vec<T>) -> Vec<T> {\n    use std::collections::HashSet;\n    let mut set: HashSet<T> = a.into_iter().collect();\n    set.extend(b.into_iter());\n    set.into_iter().collect()\n}\n\n")
	}
	if c.helpers["_union_all"] {
		out.WriteString("fn _union_all<T: Clone>(mut a: Vec<T>, b: Vec<T>) -> Vec<T> {\n    a.extend(b);\n    a\n}\n\n")
	}
	if c.helpers["_except"] {
		out.WriteString("fn _except<T: Eq + std::hash::Hash + Clone>(a: Vec<T>, b: Vec<T>) -> Vec<T> {\n    use std::collections::HashSet;\n    let set: HashSet<T> = b.into_iter().collect();\n    a.into_iter().filter(|x| !set.contains(x)).collect()\n}\n\n")
	}
	if c.helpers["_intersect"] {
		out.WriteString("fn _intersect<T: Eq + std::hash::Hash + Clone>(a: Vec<T>, b: Vec<T>) -> Vec<T> {\n    use std::collections::HashSet;\n    let set: HashSet<T> = b.into_iter().collect();\n    a.into_iter().filter(|x| set.contains(x)).collect()\n}\n\n")
	}
	out.Write(c.buf.Bytes())
	return out.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val + ";")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	if l.Value == nil {
		if l.Type == nil {
			return fmt.Errorf("let without value at line %d", l.Pos.Line)
		}
		typ := rustType(l.Type)
		c.writeln(fmt.Sprintf("let %s: %s = %s;", l.Name, typ, rustDefault(typ)))
		return nil
	}
	val, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	if l.Type != nil {
		typ := rustType(l.Type)
		c.writeln(fmt.Sprintf("let %s: %s = %s;", l.Name, typ, val))
	} else {
		c.writeln(fmt.Sprintf("let %s = %s;", l.Name, val))
	}
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	if v.Value == nil {
		if v.Type == nil {
			return fmt.Errorf("var without value at line %d", v.Pos.Line)
		}
		typ := rustType(v.Type)
		c.writeln(fmt.Sprintf("let mut %s: %s = %s;", v.Name, typ, rustDefault(typ)))
		return nil
	}
	val, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	if v.Type != nil {
		typ := rustType(v.Type)
		c.writeln(fmt.Sprintf("let mut %s: %s = %s;", v.Name, typ, val))
	} else {
		c.writeln(fmt.Sprintf("let mut %s = %s;", v.Name, val))
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	target := a.Name
	prefix := &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: a.Name}}}}
	for _, idx := range a.Index {
		idxStr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		t := types.TypeOfPostfixBasic(prefix, c.env)
		if types.IsMapType(t) {
			target = fmt.Sprintf("%s[%s]", target, idxStr)
		} else {
			target = fmt.Sprintf("%s[%s as usize]", target, idxStr)
		}
		prefix.Value.Ops = append(prefix.Value.Ops, &parser.PostfixOp{Index: idx})
	}
	for _, f := range a.Field {
		target = fmt.Sprintf("%s.%s", target, f.Name)
		prefix.Value.Ops = append(prefix.Value.Ops, &parser.PostfixOp{Field: f})
	}
	c.writeln(fmt.Sprintf("%s = %s;", target, val))
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if " + cond + " {")
	c.indent++
	for _, s := range i.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	if i.Else != nil {
		c.writeln("} else {")
		c.indent++
		for _, s := range i.Else {
			if err := c.compileStmt(s); err != nil {
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
		c.writeln(fmt.Sprintf("for %s in %s..%s {", f.Name, src, end))
	} else {
		c.writeln(fmt.Sprintf("for %s in %s {", f.Name, src))
	}
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " {")
	c.indent++
	for _, s := range w.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileTypeDecl(td *parser.TypeDecl) error {
	if len(td.Variants) > 0 {
		return fmt.Errorf("variant types not supported")
	}
	st := types.StructType{Name: td.Name, Fields: map[string]types.Type{}, Order: []string{}}
	c.writeln(fmt.Sprintf("struct %s {", td.Name))
	c.indent++
	for _, m := range td.Members {
		if m.Field == nil {
			return fmt.Errorf("unsupported type member")
		}
		typ := rustType(m.Field.Type)
		st.Fields[m.Field.Name] = types.ResolveTypeRef(m.Field.Type, c.env)
		st.Order = append(st.Order, m.Field.Name)
		c.writeln(fmt.Sprintf("%s: %s,", m.Field.Name, typ))
	}
	c.indent--
	c.writeln("}")
	c.structs[td.Name] = st
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = fmt.Sprintf("%s: %s", p.Name, rustType(p.Type))
	}
	retTy := rustType(f.Return)
	c.writeln(fmt.Sprintf("fn %s(%s) -> %s {", f.Name, strings.Join(params, ", "), retTy))
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	leftAST := b.Left
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "in":
			if c.env != nil {
				rt := types.TypeOfPostfix(op.Right, c.env)
				switch {
				case types.IsStringType(rt):
					if !c.unaryIsString(leftAST) {
						return "", fmt.Errorf("in type mismatch")
					}
					res = fmt.Sprintf("%s.contains(%s)", r, res)
				case types.IsListType(rt):
					res = fmt.Sprintf("%s.contains(&%s)", r, res)
				case types.IsMapType(rt):
					res = fmt.Sprintf("%s.contains_key(&%s)", r, res)
				default:
					return "", fmt.Errorf("in unsupported")
				}
			} else {
				return "", fmt.Errorf("in unsupported")
			}
		case "+":
			if c.unaryIsString(leftAST) || c.postfixIsString(op.Right) {
				res = fmt.Sprintf("format!(\"{}{}\", %s, %s)", res, r)
			} else {
				res = fmt.Sprintf("%s + %s", res, r)
			}
		case "union":
			if op.All {
				c.helpers["_union_all"] = true
				res = fmt.Sprintf("_union_all(%s, %s)", res, r)
			} else {
				c.helpers["_union"] = true
				res = fmt.Sprintf("_union(%s, %s)", res, r)
			}
		case "except":
			c.helpers["_except"] = true
			res = fmt.Sprintf("_except(%s, %s)", res, r)
		case "intersect":
			c.helpers["_intersect"] = true
			res = fmt.Sprintf("_intersect(%s, %s)", res, r)
		default:
			res = fmt.Sprintf("%s %s %s", res, op.Op, r)
		}
		leftAST = &parser.Unary{Value: op.Right}
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = u.Ops[i] + val
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for opIndex, op := range p.Ops {
		switch {
		case op.Cast != nil:
			rustTy := rustType(op.Cast.Type)
			if p.Target.Map != nil && opIndex == 0 && len(p.Ops) == 1 && op.Cast.Type.Simple != nil {
				lit, err := c.compileStructLiteralFromMap(*op.Cast.Type.Simple, p.Target.Map)
				if err == nil {
					val = lit
					break
				}
			}
			switch rustTy {
			case "i32":
				val = fmt.Sprintf("%s.parse::<i32>().unwrap()", val)
			default:
				return "", fmt.Errorf("unsupported cast to %s", rustTy)
			}
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.End != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
				return "", fmt.Errorf("slicing not supported")
			}
			if op.Index.Start == nil {
				return "", fmt.Errorf("missing index")
			}
			idxVal, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			prefix := &parser.Unary{Value: &parser.PostfixExpr{Target: p.Target, Ops: p.Ops[:opIndex]}}
			t := types.TypeOfPostfixBasic(prefix, c.env)
			if types.IsMapType(t) {
				val = fmt.Sprintf("%s[%s]", val, idxVal)
			} else {
				val = fmt.Sprintf("%s[%s as usize]", val, idxVal)
			}
		case op.Field != nil:
			val = fmt.Sprintf("%s.%s", val, op.Field.Name)
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
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "vec![" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return p.Selector.Root, nil
		}
		return p.Selector.Root + "." + strings.Join(p.Selector.Tail, "."), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	fmt.Fprintf(&b, "if %s { %s }", cond, thenExpr)
	if ie.ElseIf != nil {
		elseStr, err := c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
		b.WriteString(" else ")
		b.WriteString(elseStr)
	} else if ie.Else != nil {
		elseExpr, err := c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
		fmt.Fprintf(&b, " else { %s }", elseExpr)
	}
	return b.String(), nil
}

func (c *Compiler) unaryIsString(u *parser.Unary) bool {
	if c.env == nil {
		return false
	}
	t := types.TypeOfUnary(u, c.env)
	return types.IsStringType(t)
}

func (c *Compiler) postfixIsString(pf *parser.PostfixExpr) bool {
	if c.env == nil {
		return false
	}
	t := types.TypeOfPostfix(pf, c.env)
	return types.IsStringType(t)
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("%s: %s", p.Name, rustType(p.Type))
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("Box::new(move |%s| %s)", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if q.Group != nil || len(q.Joins) > 0 {
		return "", fmt.Errorf("query features not supported")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	child := types.NewEnv(c.env)
	if lt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.ListType); ok {
		child.SetVar(q.Var, lt.Elem, true)
	} else {
		child.SetVar(q.Var, types.AnyType{}, true)
	}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = s
		if lt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, true)
		} else {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
	}
	orig := c.env
	c.env = child
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	c.env = orig

	tmp := c.newTmp()
	var b strings.Builder
	b.WriteString("{ let mut ")
	b.WriteString(tmp)
	b.WriteString(" = Vec::new();")
	b.WriteString(fmt.Sprintf("for &%s in &%s {", q.Var, src))
	for i, fs := range fromSrcs {
		b.WriteString(fmt.Sprintf(" for &%s in &%s {", q.Froms[i].Var, fs))
	}
	if cond != "" {
		b.WriteString(" if !(" + cond + ") { continue; }")
	}
	if sortExpr != "" {
		item := c.newTmp()
		key := c.newTmp()
		b.WriteString(" let " + item + " = " + sel + ";")
		b.WriteString(" let " + key + " = " + sortExpr + ";")
		b.WriteString(" " + tmp + ".push((" + key + ", " + item + "));")
	} else {
		b.WriteString(" " + tmp + ".push(" + sel + ");")
	}
	for range fromSrcs {
		b.WriteString(" }")
	}
	b.WriteString(" }")
	if sortExpr != "" {
		b.WriteString(" " + tmp + ".sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap());")
		res := c.newTmp()
		b.WriteString(" let mut " + res + " = Vec::new();")
		b.WriteString(" for p in " + tmp + " { " + res + ".push(p.1); }")
		tmp = res
	}
	if skipExpr != "" || takeExpr != "" {
		start := "0usize"
		if skipExpr != "" {
			start = skipExpr + " as usize"
		}
		end := ""
		if takeExpr != "" {
			if skipExpr != "" {
				end = "(" + skipExpr + " + " + takeExpr + ") as usize"
			} else {
				end = takeExpr + " as usize"
			}
		} else {
			end = tmp + ".len()"
		}
		b.WriteString(" let " + tmp + " = " + tmp + "[" + start + ".." + end + "].to_vec();")
	}
	if q.Distinct {
		uniq := c.newTmp()
		b.WriteString(" let mut set = std::collections::HashSet::new();")
		b.WriteString(" let mut " + uniq + " = Vec::new();")
		b.WriteString(" for v in " + tmp + ".into_iter() { if set.insert(v.clone()) { " + uniq + ".push(v); } }")
		b.WriteString(" let " + tmp + " = " + uniq + ";")
	}
	b.WriteString(" " + tmp + " }")
	return b.String(), nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	var b strings.Builder
	b.WriteString("{ let mut m = std::collections::HashMap::new();")
	for _, it := range m.Items {
		var k string
		if name, ok := c.simpleIdent(it.Key); ok {
			k = fmt.Sprintf("\"%s\"", name)
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
		fmt.Fprintf(&b, " m.insert(%s, %s);", k, v)
	}
	b.WriteString(" m }")
	return b.String(), nil
}

func (c *Compiler) compileStructLiteral(sl *parser.StructLiteral) (string, error) {
	fields := make([]string, len(sl.Fields))
	for i, f := range sl.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", f.Name, v)
	}
	return fmt.Sprintf("%s { %s }", sl.Name, strings.Join(fields, ", ")), nil
}

func (c *Compiler) compileStructLiteralFromMap(name string, m *parser.MapLiteral) (string, error) {
	st, ok := c.structs[name]
	if !ok {
		if c.env != nil {
			if s, ok2 := c.env.GetStruct(name); ok2 {
				st = s
				c.structs[name] = st
				ok = true
			}
		}
	}
	if !ok {
		return "", fmt.Errorf("unknown struct %s", name)
	}
	fieldMap := map[string]*parser.Expr{}
	for _, it := range m.Items {
		if str, ok := c.simpleString(it.Key); ok {
			fieldMap[str] = it.Value
		}
	}
	fields := make([]string, len(st.Order))
	for i, f := range st.Order {
		expr, ok := fieldMap[f]
		if !ok {
			return "", fmt.Errorf("missing field %s", f)
		}
		v, err := c.compileExpr(expr)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", f, v)
	}
	return fmt.Sprintf("%s { %s }", name, strings.Join(fields, ", ")), nil
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
	if typ, err := c.env.GetVar(call.Func); err == nil {
		if ft, ok := typ.(types.FuncType); ok && !ft.Variadic && len(args) < len(ft.Params) {
			missing := len(ft.Params) - len(args)
			params := make([]string, missing)
			for i := range params {
				params[i] = c.newTmp()
			}
			callArgs := append(append([]string{}, args...), params...)
			return fmt.Sprintf("|%s| %s(%s)", strings.Join(params, ", "), call.Func, strings.Join(callArgs, ", ")), nil
		}
	}
	switch call.Func {
	case "print":
		fmtStr := strings.TrimSpace(strings.Repeat("{:?} ", len(args)))
		return fmt.Sprintf("println!(\"%s\", %s)", fmtStr, strings.Join(args, ", ")), nil
	case "append":
		c.helpers["append"] = true
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("append(%s, %s)", args[0], args[1]), nil
	case "avg":
		c.helpers["avg"] = true
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("avg(%s)", args[0]), nil
	case "len", "count":
		if len(args) != 1 {
			return "", fmt.Errorf("%s expects 1 arg", call.Func)
		}
		return fmt.Sprintf("%s.len()", args[0]), nil
	case "sum":
		c.helpers["sum"] = true
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("sum(%s)", args[0]), nil
	case "min":
		c.helpers["min"] = true
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		return fmt.Sprintf("min(%s)", args[0]), nil
	case "max":
		c.helpers["max"] = true
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		return fmt.Sprintf("max(%s)", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		return fmt.Sprintf("(%s.len() > 0)", args[0]), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		} else {
			return "false"
		}
	default:
		return "()"
	}
}

func rustType(t *parser.TypeRef) string {
	if t == nil {
		return "i32"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "i32"
		case "bool":
			return "bool"
		case "float":
			return "f64"
		case "string":
			return "&'static str"
		}
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = rustType(p)
		}
		ret := rustType(t.Fun.Return)
		return fmt.Sprintf("Box<dyn Fn(%s) -> %s>", strings.Join(params, ", "), ret)
	}
	return "i32"
}

func rustDefault(typ string) string {
	switch typ {
	case "i32":
		return "0"
	case "bool":
		return "false"
	case "f64":
		return "0.0"
	case "&'static str":
		return "\"\""
	default:
		return fmt.Sprintf("%s::default()", typ)
	}
}

func (c *Compiler) simpleString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return "", false
	}
	p := u.Value
	if p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Str == nil || len(p.Ops) > 0 {
		return "", false
	}
	return *p.Target.Lit.Str, true
}

func (c *Compiler) simpleIdent(e *parser.Expr) (string, bool) {
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

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
