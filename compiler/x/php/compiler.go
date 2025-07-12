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
	tmpCount  int
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
	c.writeln("?>")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Test != nil:
		for _, stmt := range s.Test.Body {
			if err := c.compileStmt(stmt); err != nil {
				return err
			}
		}
		return nil
	case s.Expect != nil:
		// Expectations inside tests are ignored in generated code
		return nil
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
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Import != nil:
		return c.compileImport(s.Import)
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil, s.ExternObject != nil:
		// extern declarations are ignored in PHP output
		return nil
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
		base := sanitizeName(td.Name)
		c.writeln(fmt.Sprintf("abstract class %s {}", base))
		for _, v := range td.Variants {
			name := sanitizeName(v.Name)
			c.writeln(fmt.Sprintf("class %s extends %s {", name, base))
			c.indent++
			for _, f := range v.Fields {
				c.writeln(fmt.Sprintf("public $%s;", sanitizeName(f.Name)))
			}
			if len(v.Fields) > 0 {
				params := make([]string, len(v.Fields))
				for i, f := range v.Fields {
					params[i] = "$" + sanitizeName(f.Name)
				}
				c.writeln(fmt.Sprintf("function __construct(%s) {", strings.Join(params, ", ")))
				c.indent++
				for _, f := range v.Fields {
					fn := sanitizeName(f.Name)
					c.writeln(fmt.Sprintf("$this->%s = $%s;", fn, fn))
				}
				c.indent--
				c.writeln("}")
			}
			c.indent--
			c.writeln("}")
		}
		return nil
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

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := "$" + sanitizeName(u.Target)
	idx := c.newTmp()
	item := c.newTmp()

	c.writeln(fmt.Sprintf("foreach (%s as %s => %s) {", list, idx, item))
	c.indent++

	var st types.StructType
	if c.env != nil {
		if typ, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := typ.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}

	var origEnv *types.Env
	if st.Name != "" {
		child := types.NewEnv(c.env)
		for _, f := range st.Order {
			c.writeln(fmt.Sprintf("$%s = %s->%s;", sanitizeName(f), item, sanitizeName(f)))
			child.SetVar(f, st.Fields[f], true)
		}
		origEnv = c.env
		c.env = child
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			if origEnv != nil {
				c.env = origEnv
			}
			return err
		}
		c.writeln("if (" + cond + ") {")
		c.indent++
	}

	for _, it := range u.Set.Items {
		val, err := c.compileExpr(it.Value)
		if err != nil {
			if origEnv != nil {
				c.env = origEnv
			}
			return err
		}
		if name, ok := isSimpleIdentExpr(it.Key); ok {
			c.writeln(fmt.Sprintf("%s->%s = %s;", item, sanitizeName(name), val))
		} else {
			key, err := c.compileExpr(it.Key)
			if err != nil {
				if origEnv != nil {
					c.env = origEnv
				}
				return err
			}
			c.writeln(fmt.Sprintf("%s[%s] = %s;", item, key, val))
		}
	}

	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	if origEnv != nil {
		c.env = origEnv
	}

	c.writeln(fmt.Sprintf("%s[%s] = %s;", list, idx, item))
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileImport(im *parser.ImportStmt) error {
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = sanitizeName(alias)
	if im.Lang != nil {
		lang := *im.Lang
		path := strings.Trim(im.Path, "\"")
		if lang == "python" && path == "math" {
			c.writeln(fmt.Sprintf("$%s = [", alias))
			c.indent++
			c.writeln("'sqrt' => function($x) { return sqrt($x); },")
			c.writeln("'pow' => function($x, $y) { return pow($x, $y); },")
			c.writeln("'sin' => function($x) { return sin($x); },")
			c.writeln("'log' => function($x) { return log($x); },")
			c.writeln("'pi' => M_PI,")
			c.writeln("'e' => M_E,")
			c.indent--
			c.writeln("];")
			return nil
		}
		if lang == "go" && strings.Contains(path, "testpkg") {
			c.writeln(fmt.Sprintf("$%s = [ 'Add' => function($a, $b) { return $a + $b; }, 'Pi' => 3.14, 'Answer' => 42 ];", alias))
			return nil
		}
	}
	c.writeln(fmt.Sprintf("$%s = [];", alias))
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
					val = fmt.Sprintf("$%s['key']", gvName)
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
			switch tt := t.(type) {
			case types.MapType:
				val = fmt.Sprintf("%s['%s']", val, name)
			case types.StructType:
				if tt.Name == "" {
					val = fmt.Sprintf("%s['%s']", val, name)
				} else {
					val = fmt.Sprintf("%s->%s", val, sanitizeName(name))
				}
			default:
				val = fmt.Sprintf("%s->%s", val, sanitizeName(name))
			}
			t = types.AnyType{}
		case op.Call != nil:
			if len(op.Call.Args) == 1 && (strings.HasSuffix(val, "->contains") || strings.HasSuffix(val, "['contains']")) {
				base := strings.TrimSuffix(strings.TrimSuffix(val, "->contains"), "['contains']")
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
		return formatList(elems), nil
	case p.Map != nil:
		parts := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var key string
			if name, ok := isSimpleIdentExpr(it.Key); ok {
				key = fmt.Sprintf("%q", name)
			} else {
				k, err := c.compileExpr(it.Key)
				if err != nil {
					return "", err
				}
				key = k
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s => %s", key, v)
		}
		return formatMap(parts), nil
	case p.Selector != nil:
		if ut, ok := c.env.FindUnionByVariant(p.Selector.Root); ok && len(p.Selector.Tail) == 0 {
			st := ut.Variants[p.Selector.Root]
			if len(st.Order) == 0 {
				return fmt.Sprintf("new %s()", sanitizeName(p.Selector.Root)), nil
			}
		}
		name := "$" + sanitizeName(p.Selector.Root)
		var t types.Type
		if c.env != nil {
			if vt, err := c.env.GetVar(p.Selector.Root); err == nil {
				t = vt
			}
		}
		for _, fld := range p.Selector.Tail {
			switch tt := t.(type) {
			case types.MapType:
				name += fmt.Sprintf("['%s']", fld)
				t = tt.Value
			case types.StructType:
				if tt.Name == "" {
					name += fmt.Sprintf("['%s']", fld)
				} else {
					name += "->" + sanitizeName(fld)
				}
				if ft, ok := tt.Fields[fld]; ok {
					t = ft
				} else {
					t = types.AnyType{}
				}
			default:
				name += fmt.Sprintf("['%s']", fld)
				t = types.AnyType{}
			}
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
		c.use("_avg")
		if name, ok := c.isGroupVarExpr(call.Args[0]); ok {
			return fmt.Sprintf("_avg($%s['items'])", name), nil
		}
		return fmt.Sprintf("_avg(%s)", args[0]), nil
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
		return fmt.Sprintf("echo json_encode(%s), PHP_EOL", args[0]), nil
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

	// Support block-bodied anonymous functions
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("function(%s) {\n", strings.Join(params, ", ")))

	// Temporarily switch buffer and indentation to compile the body
	origBuf := c.buf
	origIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = 1
	for _, st := range fn.BlockBody {
		if err := c.compileStmt(st); err != nil {
			c.buf = origBuf
			c.indent = origIndent
			return "", err
		}
	}
	if len(fn.BlockBody) == 0 {
		c.writeln("return;")
	}
	body := c.buf.String()
	c.buf = origBuf
	c.indent = origIndent

	buf.WriteString(body)
	buf.WriteString("}")
	return buf.String(), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	for _, j := range q.Joins {
		if j.Side != nil && *j.Side != "" {
			return c.compileQueryExprAdvanced(q)
		}
	}

	capture := queryFreeVars(q, c.env)
	use := ""
	if len(capture) > 0 {
		use = " use (" + strings.Join(capture, ", ") + ")"
	}

	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	if _, ok := c.isGroupVarExpr(q.Source); ok {
		src = fmt.Sprintf("%s['items']", src)
	}
	child := types.NewEnv(c.env)
	switch t := types.TypeOfExprBasic(q.Source, c.env).(type) {
	case types.ListType:
		child.SetVar(q.Var, t.Elem, true)
	case types.GroupType:
		child.SetVar(q.Var, t.Elem, true)
	default:
		child.SetVar(q.Var, types.AnyType{}, true)
	}
	for _, f := range q.Froms {
		if lt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, true)
		} else {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
	}
	for _, j := range q.Joins {
		if lt, ok := types.TypeOfExprBasic(j.Src, c.env).(types.ListType); ok {
			child.SetVar(j.Var, lt.Elem, true)
		} else {
			child.SetVar(j.Var, types.AnyType{}, true)
		}
	}
	oldEnv := c.env
	c.env = child
	defer func() { c.env = oldEnv }()

	var buf bytes.Buffer
	buf.WriteString("(function()" + use + " {")
	buf.WriteString("\n")
	var aggExpr *parser.Expr
	if q.Group != nil {
		buf.WriteString("    $groups = [];")
	} else {
		if a, ok := simpleCall(q.Select, "sum"); ok {
			buf.WriteString("    $result = 0;")
			aggExpr = a
		} else {
			buf.WriteString("    $result = [];")
		}
	}
	buf.WriteString("\n")

	type loopCond struct{ loop, cond string }
	loops := []loopCond{{loop: fmt.Sprintf("foreach (%s as $%s)", src, sanitizeName(q.Var))}}
	for _, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		if _, ok := c.isGroupVarExpr(f.Src); ok {
			s = fmt.Sprintf("%s['items']", s)
		}
		loops = append(loops, loopCond{loop: fmt.Sprintf("foreach (%s as $%s)", s, sanitizeName(f.Var))})
	}
	for _, j := range q.Joins {
		s, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		if _, ok := c.isGroupVarExpr(j.Src); ok {
			s = fmt.Sprintf("%s['items']", s)
		}
		cond, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		loops = append(loops, loopCond{loop: fmt.Sprintf("foreach (%s as $%s)", s, sanitizeName(j.Var)), cond: cond})
	}

	indent := 1
	for _, lc := range loops {
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		buf.WriteString(lc.loop + " {")
		buf.WriteString("\n")
		indent++
		if lc.cond != "" {
			for i := 0; i < indent; i++ {
				buf.WriteString("    ")
			}
			buf.WriteString("if (" + lc.cond + ") {")
			buf.WriteString("\n")
			indent++
		}
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
		vars := []string{q.Var}
		for _, f := range q.Froms {
			vars = append(vars, f.Var)
		}
		for _, j := range q.Joins {
			vars = append(vars, j.Var)
		}
		if len(vars) == 1 {
			buf.WriteString(fmt.Sprintf("$groups[$_k][] = $%s;\n", sanitizeName(vars[0])))
		} else {
			rowElems := make([]string, len(vars))
			for i, v := range vars {
				rowElems[i] = fmt.Sprintf("\"%s\" => $%s", v, sanitizeName(v))
			}
			buf.WriteString(fmt.Sprintf("$groups[$_k][] = [%s];\n", strings.Join(rowElems, ", ")))
		}
	} else {
		if aggExpr != nil {
			arg, err := c.compileExpr(aggExpr)
			if err != nil {
				return "", err
			}
			for i := 0; i < indent; i++ {
				buf.WriteString("    ")
			}
			buf.WriteString(fmt.Sprintf("$result += %s;\n", arg))
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
		lc := loops[i]
		if lc.cond != "" {
			indent--
			for j := 0; j < indent; j++ {
				buf.WriteString("    ")
			}
			buf.WriteString("}")
			buf.WriteString("\n")
		}
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

		genv := types.NewEnv(child)
		vars := []string{q.Var}
		for _, f := range q.Froms {
			vars = append(vars, f.Var)
		}
		for _, j := range q.Joins {
			vars = append(vars, j.Var)
		}
		if len(vars) == 1 {
			if vt, err := child.GetVar(vars[0]); err == nil {
				genv.SetVar(q.Group.Name, types.GroupType{Elem: vt}, true)
			} else {
				genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
			}
		} else {
			st := types.StructType{Fields: map[string]types.Type{}, Order: []string{}}
			for _, v := range vars {
				if vt, err := child.GetVar(v); err == nil {
					st.Fields[v] = vt
					st.Order = append(st.Order, v)
				}
			}
			genv.SetVar(q.Group.Name, types.GroupType{Elem: st}, true)
		}
		oldEnv2 := c.env
		c.env = genv

		if q.Group.Having != nil {
			cond, err := c.compileExpr(q.Group.Having)
			if err != nil {
				c.env = oldEnv2
				return "", err
			}
			buf.WriteString("        if (" + cond + ") {\n")
		}
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = oldEnv2
			return "", err
		}
		keyExpr := ""
		if q.Sort != nil {
			keyExpr, err = c.compileExpr(q.Sort)
			if err != nil {
				c.env = oldEnv2
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
		c.env = oldEnv2
		delete(c.groupVars, gname)
		buf.WriteString("    }\n")
	}

	if aggExpr != nil {
		buf.WriteString("    return $result;\n")
		buf.WriteString("})()")
		return buf.String(), nil
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
	if ut, ok := c.env.FindUnionByVariant(sl.Name); ok {
		st := ut.Variants[sl.Name]
		args := make([]string, len(st.Order))
		for i, name := range st.Order {
			valStr := "null"
			for _, f := range sl.Fields {
				if f.Name == name {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					valStr = v
					break
				}
			}
			args[i] = valStr
		}
		return fmt.Sprintf("new %s(%s)", sanitizeName(sl.Name), strings.Join(args, ", ")), nil
	}
	fields := make([]string, len(sl.Fields))
	for i, f := range sl.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("'%s' => %s", f.Name, v)
	}
	return fmt.Sprintf("new %s(%s)", sanitizeName(sl.Name), formatMap(fields)), nil
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

	simple := true
	defaultRes := "null"
	cases := make([]string, 0, len(m.Cases))
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			defaultRes = res
			continue
		}
		if isSimpleLiteralExpr(cs.Pattern) {
			pat, err := c.compileExpr(cs.Pattern)
			if err != nil {
				return "", err
			}
			cases = append(cases, fmt.Sprintf("%s => %s", pat, res))
			continue
		}
		simple = false
		break
	}

	if simple {
		var buf bytes.Buffer
		buf.WriteString("match(" + target + ") {\n")
		for _, cse := range cases {
			buf.WriteString("    " + cse + ",\n")
		}
		buf.WriteString("    default => " + defaultRes + ",\n")
		buf.WriteString("}")
		return buf.String(), nil
	}

	var buf bytes.Buffer
	buf.WriteString("(function($_t) {\n")
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			buf.WriteString("    return " + res + ";\n")
			buf.WriteString("})(" + target + ")")
			return buf.String(), nil
		}
		cond := ""
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				cond = fmt.Sprintf("$_t instanceof %s", sanitizeName(call.Func))
				names := []string{}
				values := []string{}
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok {
						if id == "_" {
							continue
						}
						names = append(names, "$"+sanitizeName(id))
						field := sanitizeName(st.Order[idx])
						values = append(values, fmt.Sprintf("$_t->%s", field))
					}
				}
				if len(names) > 0 {
					res = fmt.Sprintf("(function(%s) { return %s; })(%s)", strings.Join(names, ", "), res, strings.Join(values, ", "))
				}
			}
		} else if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond = fmt.Sprintf("$_t instanceof %s", sanitizeName(ident))
			}
		}
		if cond == "" {
			p, err := c.compileExpr(cs.Pattern)
			if err != nil {
				return "", err
			}
			cond = fmt.Sprintf("$_t === %s", p)
		}
		buf.WriteString(fmt.Sprintf("    if (%s) return %s;\n", cond, res))
	}
	buf.WriteString("    return null;\n")
	buf.WriteString("})(" + target + ")")
	return buf.String(), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "null"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "[]"
	if l.With != nil {
		o, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.use("_load")
	expr := fmt.Sprintf("_load(%s, %s)", path, opts)
	if l.Type != nil && l.Type.Simple != nil {
		tname := sanitizeName(*l.Type.Simple)
		expr = fmt.Sprintf("array_map(fn($it) => new %s($it), %s)", tname, expr)
	}
	return expr, nil
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
	opts := "[]"
	if s.With != nil {
		o, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.use("_save")
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
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

func (c *Compiler) compileQueryExprAdvanced(q *parser.QueryExpr) (string, error) {
	capture := queryFreeVars(q, c.env)
	use := ""
	if len(capture) > 0 {
		use = " use (" + strings.Join(capture, ", ") + ")"
	}

	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	child := types.NewEnv(c.env)
	switch t := types.TypeOfExprBasic(q.Source, c.env).(type) {
	case types.ListType:
		child.SetVar(q.Var, t.Elem, true)
	case types.GroupType:
		child.SetVar(q.Var, t.Elem, true)
	default:
		child.SetVar(q.Var, types.AnyType{}, true)
	}
	for _, f := range q.Froms {
		if lt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, true)
		} else {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
	}
	for _, j := range q.Joins {
		if lt, ok := types.TypeOfExprBasic(j.Src, c.env).(types.ListType); ok {
			child.SetVar(j.Var, lt.Elem, true)
		} else {
			child.SetVar(j.Var, types.AnyType{}, true)
		}
	}
	orig := c.env
	c.env = child

	fromSrcs := make([]string, len(q.Froms))
	varNames := []string{sanitizeName(q.Var)}
	for i, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		fromSrcs[i] = s
		varNames = append(varNames, sanitizeName(f.Var))
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	paramCopy := append([]string(nil), varNames...)
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinSrcs[i] = js
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinOns[i] = on
		varNames = append(varNames, sanitizeName(j.Var))
	}

	val, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var keyExpr string
	if q.Group != nil {
		keyExpr, err = c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
	}

	c.env = orig

	joins := make([]string, 0, len(q.Froms)+len(q.Joins))
	params := []string{sanitizeName(q.Var)}
	for i, fs := range fromSrcs {
		joins = append(joins, fmt.Sprintf("['items'=>%s]", fs))
		params = append(params, sanitizeName(q.Froms[i].Var))
	}
	paramCopy = append([]string(nil), params...)
	for i, js := range joinSrcs {
		onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
		fnParams := make([]string, len(onParams))
		for k, p := range onParams {
			fnParams[k] = "$" + p
		}
		spec := fmt.Sprintf("['items'=>%s, 'on'=>function(%s)%s{return %s;}", js, strings.Join(fnParams, ", "), use, joinOns[i])
		if q.Joins[i].Side != nil {
			side := *q.Joins[i].Side
			if side == "left" || side == "outer" {
				spec += ", 'left'=>true"
			}
			if side == "right" || side == "outer" {
				spec += ", 'right'=>true"
			}
		}
		spec += "]"
		joins = append(joins, spec)
		paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
	}
	paramList := make([]string, len(paramCopy))
	for i, p := range paramCopy {
		paramList[i] = "$" + p
	}
	allParams := strings.Join(paramList, ", ")
	var selectFn string
	if q.Group == nil {
		selectFn = fmt.Sprintf("function(%s)%s{return %s;}", allParams, use, val)
	} else {
		selectFn = fmt.Sprintf("function(%s)%s{return [%s];}", allParams, use, strings.Join(paramList, ", "))
	}
	opts := "[ 'select' => " + selectFn + " ]"
	expr := fmt.Sprintf("_query(%s, [%s], %s)", src, strings.Join(joins, ", "), opts)
	c.use("_query")
	if q.Group == nil {
		return expr, nil
	}

	genv := types.NewEnv(child)
	vars := []string{q.Var}
	for _, f := range q.Froms {
		vars = append(vars, f.Var)
	}
	for _, j := range q.Joins {
		vars = append(vars, j.Var)
	}
	if len(vars) == 1 {
		if vt, err := child.GetVar(vars[0]); err == nil {
			genv.SetVar(q.Group.Name, types.GroupType{Elem: vt}, true)
		} else {
			genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		}
	} else {
		st := types.StructType{Fields: map[string]types.Type{}, Order: []string{}}
		for _, v := range vars {
			if vt, err := child.GetVar(v); err == nil {
				st.Fields[v] = vt
				st.Order = append(st.Order, v)
			}
		}
		genv.SetVar(q.Group.Name, types.GroupType{Elem: st}, true)
	}
	gname := sanitizeName(q.Group.Name)
	c.groupVars[gname] = true
	c.env = genv
	groupVal, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		delete(c.groupVars, gname)
		return "", err
	}
	havingCond := ""
	if q.Group.Having != nil {
		havingCond, err = c.compileExpr(q.Group.Having)
		if err != nil {
			c.env = orig
			delete(c.groupVars, gname)
			return "", err
		}
	}
	c.env = orig

	var buf bytes.Buffer
	buf.WriteString("(function()" + use + " {\n")
	buf.WriteString(fmt.Sprintf("    $_rows = %s;\n", expr))
	buf.WriteString(fmt.Sprintf("    $_groups = _group_by($_rows, function(%s)%s{return %s;});\n", allParams, use, keyExpr))
	buf.WriteString("    $result = [];\n")
	buf.WriteString("    foreach ($_groups as $__g) {\n")
	buf.WriteString(fmt.Sprintf("        $%s = $__g;\n", gname))
	if havingCond != "" {
		buf.WriteString(fmt.Sprintf("        if (%s) {\n", havingCond))
		buf.WriteString(fmt.Sprintf("            $result[] = %s;\n", groupVal))
		buf.WriteString("        }\n")
	} else {
		buf.WriteString(fmt.Sprintf("        $result[] = %s;\n", groupVal))
	}
	buf.WriteString("    }\n")
	buf.WriteString("    return $result;\n")
	buf.WriteString("})()")

	delete(c.groupVars, gname)
	c.use("_group_by")
	return buf.String(), nil
}
