//go:build slow

package phpcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler converts Mochi AST to PHP source code.
type Compiler struct {
	buf       bytes.Buffer
	indent    int
	needsJSON bool
	env       *types.Env
	helpers   map[string]bool
	groupVars map[string]bool
}

// New creates a new Compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env, groupVars: map[string]bool{}} }

// Compile translates the program to PHP code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needsJSON = false
	c.writeln("<?php")
	for _, s := range prog.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.emitRuntime()
	return c.buf.Bytes(), nil
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
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
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
		if expr != "" {
			c.writeln(expr + ";")
		}
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error { return c.compileVarStmt(l.Name, l.Value) }

func (c *Compiler) compileVar(v *parser.VarStmt) error { return c.compileVarStmt(v.Name, v.Value) }

func (c *Compiler) compileVarStmt(name string, val *parser.Expr) error {
	var value string
	if val != nil {
		var err error
		value, err = c.compileExpr(val)
		if err != nil {
			return err
		}
	} else {
		value = "null"
	}
	c.writeln(fmt.Sprintf("$%s = %s;", sanitizeName(name), value))
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	target := "$" + sanitizeName(a.Name)
	for _, idx := range a.Index {
		if idx.Start == nil || idx.Colon != nil {
			return fmt.Errorf("complex indexing not supported")
		}
		iv, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		target = fmt.Sprintf("%s[%s]", target, iv)
	}
	for _, f := range a.Field {
		target = fmt.Sprintf("%s->%s", target, sanitizeName(f.Name))
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", target, val))
	return nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = "$" + sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("function %s(%s) {", sanitizeName(fn.Name), strings.Join(params, ", ")))
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if len(fn.Body) == 0 {
		c.writeln("return;")
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	val := "null"
	if r.Value != nil {
		v, err := c.compileExpr(r.Value)
		if err != nil {
			return err
		}
		val = v
	}
	c.writeln("return " + val + ";")
	return nil
}

func (c *Compiler) compileTypeDecl(td *parser.TypeDecl) error {
	if len(td.Variants) > 0 {
		return fmt.Errorf("variant types not supported")
	}
	c.writeln(fmt.Sprintf("class %s {", sanitizeName(td.Name)))
	c.indent++
	for _, m := range td.Members {
		if m.Field != nil {
			c.writeln(fmt.Sprintf("public $%s;", sanitizeName(m.Field.Name)))
		}
	}
	c.writeln("public function __construct($fields = []) {")
	c.indent++
	for _, m := range td.Members {
		if m.Field != nil {
			orig := m.Field.Name
			c.writeln(fmt.Sprintf("$this->%s = $fields['%s'] ?? null;", sanitizeName(orig), orig))
		}
	}
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("if (" + cond + ") {")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	if len(st.Then) == 0 {
		c.writeln("//pass")
	}
	c.indent--
	curElseIf := st.ElseIf
	for curElseIf != nil {
		cond2, err := c.compileExpr(curElseIf.Cond)
		if err != nil {
			return err
		}
		c.writeln("} elseif (" + cond2 + ") {")
		c.indent++
		for _, s := range curElseIf.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		if len(curElseIf.Then) == 0 {
			c.writeln("//pass")
		}
		c.indent--
		curElseIf = curElseIf.ElseIf
	}
	if len(st.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(ws *parser.WhileStmt) error {
	cond, err := c.compileExpr(ws.Cond)
	if err != nil {
		return err
	}
	c.writeln("while (" + cond + ") {")
	c.indent++
	for _, st := range ws.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileFor(fs *parser.ForStmt) error {
	name := "$" + sanitizeName(fs.Name)
	if fs.RangeEnd != nil {
		start, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fs.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (%s = %s; %s <= %s; %s++) {", name, start, name, end, name))
	} else {
		src, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		if _, ok := c.isGroupVarExpr(fs.Source); ok {
			src = fmt.Sprintf("%s['items']", src)
		}
		if c.isMapExpr(fs.Source) {
			c.writeln(fmt.Sprintf("foreach (%s as %s => $_v) {", src, name))
		} else {
			c.writeln(fmt.Sprintf("foreach (%s as %s) {", src, name))
		}
	}
	c.indent++
	for _, st := range fs.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
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
	leftType := types.TypeOfUnary(b.Left, c.env)
	res := left
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightType := types.TypeOfPostfix(op.Right, c.env)
		opStr := op.Op
		switch opStr {
		case "&&":
			opStr = "&&"
		case "||":
			opStr = "||"
		case "in":
			switch {
			case isStringType(rightType):
				res = fmt.Sprintf("strpos(%s, %s) !== false", r, left)
			case isMapType(rightType):
				res = fmt.Sprintf("array_key_exists(%s, %s)", left, r)
			default:
				res = fmt.Sprintf("in_array(%s, %s)", left, r)
			}
			left = res
			leftType = types.BoolType{}
			continue
		case "union":
			if op.All {
				res = fmt.Sprintf("array_merge(%s, %s)", res, r)
			} else {
				res = fmt.Sprintf("array_values(array_unique(array_merge(%s, %s), SORT_REGULAR))", res, r)
			}
			left = res
			leftType = types.AnyType{}
			continue
		case "except":
			res = fmt.Sprintf("array_values(array_diff(%s, %s))", res, r)
			left = res
			leftType = types.AnyType{}
			continue
		case "intersect":
			res = fmt.Sprintf("array_values(array_intersect(%s, %s))", res, r)
			left = res
			leftType = types.AnyType{}
			continue
		case "+":
			if isStringType(leftType) || isStringType(rightType) {
				opStr = "."
				leftType = types.StringType{}
			} else {
				leftType = types.AnyType{}
			}
		default:
			leftType = types.AnyType{}
		}
		res = fmt.Sprintf("%s %s %s", res, opStr, r)
		left = res
	}
	return res, nil
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
			val = "!" + val
		default:
			return "", fmt.Errorf("unsupported unary op %s", u.Ops[i])
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	gvName := ""
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		name := sanitizeName(p.Target.Selector.Root)
		if c.groupVars[name] {
			gvName = name
		}
	}
	t := types.TypeOfPrimary(p.Target, c.env)
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				length := ""
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					length = fmt.Sprintf("%s - %s", e, start)
				}
				if _, ok := t.(types.StringType); ok {
					if length == "" {
						val = fmt.Sprintf("substr(%s, %s)", val, start)
					} else {
						val = fmt.Sprintf("substr(%s, %s, %s)", val, start, length)
					}
					t = types.StringType{}
				} else {
					if length == "" {
						val = fmt.Sprintf("array_slice(%s, %s)", val, start)
					} else {
						val = fmt.Sprintf("array_slice(%s, %s, %s)", val, start, length)
					}
					if lt, ok := t.(types.ListType); ok {
						t = lt.Elem
					} else {
						t = types.AnyType{}
					}
				}
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("empty index")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				val = fmt.Sprintf("%s[%s]", val, idx)
				switch tt := t.(type) {
				case types.ListType:
					t = tt.Elem
				case types.MapType:
					t = tt.Value
				case types.StringType:
					t = types.StringType{}
				default:
					t = types.AnyType{}
				}
			}
		case op.Field != nil:
			name := op.Field.Name
			if gvName != "" {
				if name == "key" {
					val = fmt.Sprintf("$%s_key", gvName)
					gvName = ""
					t = types.AnyType{}
					continue
				}
				if name == "items" {
					val = fmt.Sprintf("$%s['items']", gvName)
					gvName = ""
					t = types.AnyType{}
					continue
				}
			}
			val = fmt.Sprintf("%s->%s", val, sanitizeName(name))
			t = types.AnyType{}
		case op.Call != nil:
			if strings.HasSuffix(val, "->contains") && len(op.Call.Args) == 1 {
				base := strings.TrimSuffix(val, "->contains")
				arg, err := c.compileExpr(op.Call.Args[0])
				if err != nil {
					return "", err
				}
				val = fmt.Sprintf("strpos(%s, %s) !== false", base, arg)
				t = types.BoolType{}
				continue
			}
			args := make([]string, len(op.Call.Args))
			for j, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[j] = s
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			t = types.AnyType{}
		case op.Cast != nil:
			if op.Cast.Type.Simple == nil {
				return "", fmt.Errorf("unsupported cast")
			}
			typ := *op.Cast.Type.Simple
			switch typ {
			case "int":
				val = fmt.Sprintf("(int)(%s)", val)
			case "float":
				val = fmt.Sprintf("(float)(%s)", val)
			case "string":
				val = fmt.Sprintf("(string)(%s)", val)
			default:
				val = fmt.Sprintf("new %s(%s)", sanitizeName(typ), val)
			}
			t = types.AnyType{}
		default:
			return "", fmt.Errorf("unsupported postfix")
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
		parts := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s => %s", k, v)
		}
		return "[" + strings.Join(parts, ", ") + "]", nil
	case p.Selector != nil:
		name := "$" + sanitizeName(p.Selector.Root)
		for _, t := range p.Selector.Tail {
			name += "->" + sanitizeName(t)
		}
		return name, nil
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
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
	switch call.Func {
	case "print":
		return fmt.Sprintf("var_dump(%s)", strings.Join(args, ", ")), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("array_merge(%s, [%s])", args[0], args[1]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		if types.IsStringType(types.TypeOfExprBasic(call.Args[0], c.env)) {
			return fmt.Sprintf("strlen(%s)", args[0]), nil
		}
		if name, ok := c.isGroupVarExpr(call.Args[0]); ok {
			return fmt.Sprintf("count($%s['items'])", name), nil
		}
		return fmt.Sprintf("count(%s)", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		if name, ok := c.isGroupVarExpr(call.Args[0]); ok {
			return fmt.Sprintf("count($%s['items'])", name), nil
		}
		return fmt.Sprintf("count(%s)", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(count(%[1]s) ? array_sum(%[1]s)/count(%[1]s) : 0)", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("array_sum(%s)", args[0]), nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		return fmt.Sprintf("min(%s)", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		return fmt.Sprintf("max(%s)", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("substr(%s, %s, %s)", args[0], args[1], args[2]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("strval(%s)", args[0]), nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		return fmt.Sprintf("json_encode(%s)", args[0]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("array_values(%s)", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		if name, ok := c.isGroupVarExpr(call.Args[0]); ok {
			return fmt.Sprintf("count($%s['items']) > 0", name), nil
		}
		return fmt.Sprintf("count(%s) > 0", args[0]), nil
	default:
		name := sanitizeName(call.Func)
		if c.env != nil {
			if fn, ok := c.env.GetFunc(call.Func); ok {
				if len(call.Args) < len(fn.Params) {
					missing := fn.Params[len(call.Args):]
					params := make([]string, len(missing))
					for i, p := range missing {
						params[i] = "$" + sanitizeName(p.Name)
					}
					bodyArgs := append(append([]string{}, args...), params...)
					return fmt.Sprintf("function(%s) { return %s(%s); }",
						strings.Join(params, ", "), name, strings.Join(bodyArgs, ", ")), nil
				}
				return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
			}
			if _, err := c.env.GetVar(call.Func); err == nil {
				return fmt.Sprintf("$%s(%s)", name, strings.Join(args, ", ")), nil
			}
		}
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = "$" + sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("function(%s) { return %s; }", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) > 0 {
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
	for _, f := range q.Froms {
		if lt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, true)
		} else {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
	}
	oldEnv := c.env
	c.env = child
	defer func() { c.env = oldEnv }()

	var buf bytes.Buffer
	buf.WriteString("(function() {")
	buf.WriteString("\n")
	if q.Group != nil {
		buf.WriteString("    $groups = [];")
	} else {
		buf.WriteString("    $result = [];")
	}
	buf.WriteString("\n")

	loops := []string{fmt.Sprintf("foreach (%s as $%s)", src, sanitizeName(q.Var))}
	for _, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		loops = append(loops, fmt.Sprintf("foreach (%s as $%s)", s, sanitizeName(f.Var)))
	}

	indent := 1
	for _, l := range loops {
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		buf.WriteString(l + " {")
		buf.WriteString("\n")
		indent++
	}

	if q.Where != nil {
		cond, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		buf.WriteString("if (" + cond + ") {")
		buf.WriteString("\n")
		indent++
	}

	if q.Group != nil {
		if len(q.Group.Exprs) != 1 {
			return "", fmt.Errorf("group by multiple expressions not supported")
		}
		keyStr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		buf.WriteString(fmt.Sprintf("$_k = json_encode(%s);\n", keyStr))
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		buf.WriteString(fmt.Sprintf("$groups[$_k][] = $%s;\n", sanitizeName(q.Var)))
	} else {
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		keyExpr := ""
		if q.Sort != nil {
			keyExpr, err = c.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
		}
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		if keyExpr != "" {
			buf.WriteString(fmt.Sprintf("$result[] = [%s, %s];", keyExpr, sel))
		} else {
			buf.WriteString(fmt.Sprintf("$result[] = %s;", sel))
		}
		buf.WriteString("\n")
	}

	if q.Where != nil {
		indent--
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		buf.WriteString("}")
		buf.WriteString("\n")
	}

	for i := len(loops) - 1; i >= 0; i-- {
		indent--
		for j := 0; j < indent; j++ {
			buf.WriteString("    ")
		}
		buf.WriteString("}")
		buf.WriteString("\n")
	}

	if q.Group != nil {
		buf.WriteString("    $result = [];\n")
		buf.WriteString("    foreach ($groups as $_k => $__g) {\n")
		gname := sanitizeName(q.Group.Name)
		c.groupVars[gname] = true
		buf.WriteString(fmt.Sprintf("        $%s = ['key'=>json_decode($_k, true),'items'=> $__g];\n", gname))
		if q.Group.Having != nil {
			cond, err := c.compileExpr(q.Group.Having)
			if err != nil {
				return "", err
			}
			buf.WriteString("        if (" + cond + ") {\n")
		}
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		keyExpr := ""
		if q.Sort != nil {
			keyExpr, err = c.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
		}
		if keyExpr != "" {
			buf.WriteString("        $result[] = [" + keyExpr + ", " + sel + "];\n")
		} else {
			buf.WriteString("        $result[] = " + sel + ";\n")
		}
		if q.Group.Having != nil {
			buf.WriteString("        }\n")
		}
		delete(c.groupVars, gname)
		buf.WriteString("    }\n")
	}

	if q.Sort != nil {
		buf.WriteString("    usort($result, function($a, $b) { return $a[0] <=> $b[0]; });\n")
		buf.WriteString("    $result = array_map(fn($r) => $r[1], $result);\n")
	}
	if q.Skip != nil || q.Take != nil {
		start := "0"
		if q.Skip != nil {
			start, err = c.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
		}
		length := "null"
		if q.Take != nil {
			length, err = c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
		}
		buf.WriteString(fmt.Sprintf("    $result = array_slice($result, %s, %s);\n", start, length))
	}
	if q.Distinct {
		buf.WriteString("    $result = array_values(array_unique($result, SORT_REGULAR));\n")
	}
	buf.WriteString("    return $result;\n")
	buf.WriteString("})()")

	return buf.String(), nil
}

func (c *Compiler) compileStructLiteral(sl *parser.StructLiteral) (string, error) {
	fields := make([]string, len(sl.Fields))
	for i, f := range sl.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("'%s' => %s", f.Name, v)
	}
	return fmt.Sprintf("new %s([%s])", sanitizeName(sl.Name), strings.Join(fields, ", ")), nil
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

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	buf.WriteString("match(" + target + ") {")
	buf.WriteByte('\n')
	for _, cs := range m.Cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if pat == "_" {
			pat = "default"
		}
		buf.WriteString("    " + pat + " => " + res + ",\n")
	}
	buf.WriteString("}")
	return buf.String(), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	case l.Null:
		return "null"
	default:
		return "null"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
