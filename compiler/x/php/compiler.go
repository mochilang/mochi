//go:build slow

package phpcode

import (
	"bytes"
	"fmt"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

const funcPrefix = "mochi_"

// Compiler translates a Mochi AST into PHP source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	baseDir      string
	locals       map[string]bool
	funcs        map[string]bool
	methodFields map[string]bool
	structs      map[string]bool
	typeNames    map[string]bool
	helpers      map[string]bool
}

// New creates a new PHP compiler instance.
func New(env *types.Env, dir string) *Compiler {
	return &Compiler{
		env:          env,
		baseDir:      dir,
		locals:       map[string]bool{},
		funcs:        map[string]bool{},
		methodFields: nil,
		structs:      map[string]bool{},
		typeNames:    map[string]bool{},
		helpers:      map[string]bool{},
	}
}

// Compile generates PHP code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("<?php")

	c.funcs = map[string]bool{}
	c.structs = map[string]bool{}
	c.typeNames = map[string]bool{}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			c.funcs[s.Fun.Name] = true
		}
		if s.Type != nil {
			c.typeNames[s.Type.Name] = true
		}
	}

	// functions first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// type declarations
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// test blocks
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// main body
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}

	// run tests
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := funcPrefix + "test_" + sanitizeName(s.Test.Name)
			c.writeln(name + "();")
		}
	}
	if len(c.helpers) > 0 {
		c.writeln("")
		c.emitRuntime()
	}
	code := FormatPHP(c.buf.Bytes())
	if err := ValidatePHP(code); err != nil {
		return code, err
	}
	return code, nil
}

// --- Statements ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Fun != nil:
		fn := s.Fun
		params := make([]string, len(fn.Params))
		for i, p := range fn.Params {
			params[i] = "$" + sanitizeName(p.Name)
		}
		fe := &parser.FunExpr{Params: fn.Params, BlockBody: fn.Body}
		captured := freeVars(fe, params)
		captured = append(captured, "$"+sanitizeName(fn.Name))
		oldLocals := c.locals
		c.locals = map[string]bool{}
		for _, p := range fn.Params {
			c.locals[p.Name] = true
		}
		// allow recursive calls
		c.locals[fn.Name] = true
		name := sanitizeName(fn.Name)
		c.writeln(fmt.Sprintf("$%s = null;", name))
		use := ""
		if len(captured) > 0 {
			for i, v := range captured {
				captured[i] = "&" + v
			}
			use = " use (" + strings.Join(captured, ", ") + ")"
		}
		c.writeln(fmt.Sprintf("$%s = function (%s)%s {", name, strings.Join(params, ", "), use))
		c.indent++
		for _, st := range fn.Body {
			if err := c.compileStmt(st); err != nil {
				c.locals = oldLocals
				return err
			}
		}
		c.indent--
		c.writeln("};")
		c.locals = oldLocals
		c.locals[fn.Name] = true
		return nil
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val + ";")
		return nil
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.writeln("break;")
		return nil
	case s.Continue != nil:
		c.writeln("continue;")
		return nil
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
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
		return nil
	}
}
func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	muts := mutatedVars(fn.Body)
	doc := []string{}
	for i, p := range fn.Params {
		name := "$" + sanitizeName(p.Name)
		if muts[p.Name] {
			if isCompositeParam(p) {
				name = "&" + name
			}
		}
		params[i] = name
		var pt types.Type = types.AnyType{}
		if p.Type != nil {
			pt = types.ResolveTypeRef(p.Type, c.env)
		}
		doc = append(doc, fmt.Sprintf(" * @param %s $%s", typeString(pt), sanitizeName(p.Name)))
	}
	retType := "void"
	if fn.Return != nil {
		rt := types.ResolveTypeRef(fn.Return, c.env)
		retType = typeString(rt)
	}
	c.writeln("/**")
	for _, d := range doc {
		c.writeln(d)
	}
	c.writeln(" * @return " + retType)
	c.writeln(" */")
	c.writeln(fmt.Sprintf("function %s%s(%s) {", funcPrefix, sanitizeName(fn.Name), strings.Join(params, ", ")))
	oldLocals := c.locals
	oldEnv := c.env
	c.locals = map[string]bool{}
	if c.env != nil {
		c.env = types.NewEnv(c.env)
	}
	for _, p := range fn.Params {
		c.locals[p.Name] = true
		if c.env != nil {
			var pt types.Type = types.AnyType{}
			if p.Type != nil {
				pt = types.ResolveTypeRef(p.Type, oldEnv)
			} else if oldEnv != nil {
				if ft, err := oldEnv.GetVar(p.Name); err == nil {
					pt = ft
				}
			}
			c.env.SetVar(p.Name, pt, true)
		}
	}
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			c.locals = oldLocals
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.locals = oldLocals
	if c.env != nil {
		c.env = oldEnv
	}
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := funcPrefix + "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("function %s() {", name))
	oldLocals := c.locals
	c.locals = map[string]bool{}
	c.indent++

	// capture globals referenced within the test block
	fn := &parser.FunExpr{BlockBody: t.Body}
	captured := freeVars(fn, nil)
	if len(captured) > 0 {
		c.writeln("global " + strings.Join(captured, ", ") + ";")
	}

	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			c.locals = oldLocals
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.locals = oldLocals
	return nil
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	name := "$" + sanitizeName(l.Name)
	if c.methodFields != nil && c.methodFields[l.Name] {
		name = "$this->" + sanitizeName(l.Name)
	}
	val := "null"
	if l.Value != nil {
		if c.env != nil {
			c.env.SetVar(l.Name, types.AnyType{}, false)
		}
		v, err := c.compileExpr(l.Value)
		if err != nil {
			return err
		}
		val = v
	}
	var typ types.Type = types.AnyType{}
	if l.Type != nil {
		typ = types.ResolveTypeRef(l.Type, c.env)
	} else if l.Value != nil {
		typ = c.inferExprType(l.Value)
	}
	c.writeln(fmt.Sprintf("// %s: %s", sanitizeName(l.Name), typeString(typ)))
	c.writeln(fmt.Sprintf("%s = %s;", name, val))
	c.locals[l.Name] = true
	if c.env != nil {
		c.env.SetVar(l.Name, typ, false)
	}
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	msg := fmt.Sprintf("expect failed: %s", strings.ReplaceAll(expr, "\"", "'"))
	c.writeln(fmt.Sprintf("if (!(%s)) { throw new Exception(\"%s\"); }", expr, msg))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	name := "$" + sanitizeName(v.Name)
	if c.methodFields != nil && c.methodFields[v.Name] {
		name = "$this->" + sanitizeName(v.Name)
	}
	val := "null"
	if v.Value != nil {
		valExpr, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		val = valExpr
	}
	var typ types.Type = types.AnyType{}
	if v.Type != nil {
		typ = types.ResolveTypeRef(v.Type, c.env)
	} else if v.Value != nil {
		typ = c.inferExprType(v.Value)
	}
	c.writeln(fmt.Sprintf("// %s: %s", sanitizeName(v.Name), typeString(typ)))
	c.writeln(fmt.Sprintf("%s = %s;", name, val))
	c.locals[v.Name] = true
	if c.env != nil {
		c.env.SetVar(v.Name, typ, true)
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	lhs := "$" + sanitizeName(a.Name)
	if c.methodFields != nil && c.methodFields[a.Name] {
		lhs = "$this->" + sanitizeName(a.Name)
	}
	for _, idx := range a.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	rhs, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := "$" + sanitizeName(f.Name)
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for (%s = %s; %s < %s; %s++) {", name, start, name, end, name))
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		if c.isMapExpr(f.Source) {
			c.writeln(fmt.Sprintf("foreach (array_keys(%s) as %s) {", src, name))
		} else {
			c.writeln(fmt.Sprintf("foreach ((is_string(%[1]s) ? str_split(%[1]s) : %[1]s) as %s) {", src, name))
		}
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

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := "$" + sanitizeName(u.Target)
	if c.methodFields != nil && c.methodFields[u.Target] {
		list = "$this->" + sanitizeName(u.Target)
	}

	fields := map[string]bool{}
	if u.Where != nil {
		for v := range exprVarSet(u.Where) {
			fields[v] = true
		}
	}
	for _, it := range u.Set.Items {
		if k, ok := identName(it.Key); ok {
			fields[k] = true
		}
		for v := range exprVarSet(it.Value) {
			fields[v] = true
		}
	}

	c.writeln(fmt.Sprintf("for ($_i = 0; $_i < count(%s); $_i++) {", list))
	c.indent++
	c.writeln(fmt.Sprintf("$_item = %s[$_i];", list))

	for f := range fields {
		c.writeln(fmt.Sprintf("$%s = is_array($_item) ? ($_item['%s'] ?? null) : $_item->%s;", sanitizeName(f), f, sanitizeName(f)))
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			return err
		}
		c.writeln("if (" + cond + ") {")
		c.indent++
	}

	for _, it := range u.Set.Items {
		key, _ := identName(it.Key)
		val, err := c.compileExpr(it.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("if (is_array($_item)) { $_item['%s'] = %s; } else { $_item->%s = %s; }", key, val, sanitizeName(key), val))
	}

	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	c.writeln(fmt.Sprintf("%s[$_i] = $_item;", list))
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

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenVal, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	elseVal := "null"
	if ie.ElseIf != nil {
		v, err := c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
		elseVal = v
	} else if ie.Else != nil {
		v, err := c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
		elseVal = v
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenVal, elseVal), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(function($_t) {\n")
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\treturn " + res + ";\n")
			b.WriteString("})(" + target + ")")
			return b.String(), nil
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
		b.WriteString(fmt.Sprintf("\tif (%s) return %s;\n", cond, res))
	}
	b.WriteString("\treturn null;\n")
	b.WriteString("})(" + target + ")")
	return b.String(), nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		for _, v := range t.Variants {
			name := sanitizeName(v.Name)
			if c.structs[name] {
				continue
			}
			c.structs[name] = true
			c.writeln(fmt.Sprintf("class %s {", name))
			c.indent++
			if len(v.Fields) > 0 {
				fields := make([]string, len(v.Fields))
				for i, f := range v.Fields {
					fields[i] = f.Name
					c.writeln(fmt.Sprintf("public $%s;", sanitizeName(f.Name)))
				}
				c.writeln("public function __construct($fields = []) {")
				c.indent++
				for _, f := range fields {
					c.writeln(fmt.Sprintf("$this->%s = $fields['%s'] ?? null;", sanitizeName(f), f))
				}
				c.indent--
				c.writeln("}")
			} else {
				c.writeln("public function __construct() {}")
			}
			c.indent--
			c.writeln("}")
			c.writeln("")
		}
		return nil
	}
	name := sanitizeName(t.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("class %s {", name))
	c.indent++
	fields := []string{}
	for _, m := range t.Members {
		if m.Field != nil {
			fname := sanitizeName(m.Field.Name)
			fields = append(fields, m.Field.Name)
			c.writeln(fmt.Sprintf("public $%s;", fname))
		}
	}
	c.writeln("public function __construct($fields = []) {")
	c.indent++
	for _, f := range fields {
		c.writeln(fmt.Sprintf("$this->%s = $fields['%s'] ?? null;", sanitizeName(f), f))
	}
	c.indent--
	c.writeln("}")
	for _, m := range t.Members {
		if m.Method != nil {
			if err := c.compileMethod(name, m.Method); err != nil {
				return err
			}
		}
	}
	c.indent--
	c.writeln("}")
	if st, ok := c.env.GetStruct(t.Name); ok {
		for _, ft := range st.Fields {
			if sub, ok := ft.(types.StructType); ok && !c.typeNames[sub.Name] {
				c.compileStructType(sub)
			}
		}
	}
	return nil
}

func (c *Compiler) compileMethod(structName string, fun *parser.FunStmt) error {
	name := sanitizeName(fun.Name)
	params := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		params[i] = "$" + sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("public function %s(%s) {", name, strings.Join(params, ", ")))
	oldLocals := c.locals
	c.locals = map[string]bool{}
	for _, p := range fun.Params {
		c.locals[p.Name] = true
	}
	if st, ok := c.env.GetStruct(structName); ok {
		c.methodFields = make(map[string]bool, len(st.Fields))
		for fn := range st.Fields {
			c.methodFields[fn] = true
		}
	}
	c.indent++
	for _, st := range fun.Body {
		if err := c.compileStmt(st); err != nil {
			c.methodFields = nil
			c.locals = oldLocals
			return err
		}
	}
	c.indent--
	c.writeln("}")
	c.methodFields = nil
	c.locals = oldLocals
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
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, left)
	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
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

	contains := func(sl []string, s string) bool {
		for _, v := range sl {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if !contains(level, ops[i]) {
				i++
				continue
			}
			op := ops[i]
			l := operands[i]
			r := operands[i+1]
			var expr string
			if op == "in" {
				expr = fmt.Sprintf("(is_array(%[2]s) ? (array_key_exists(%[1]s, %[2]s) || in_array(%[1]s, %[2]s, true)) : (is_string(%[2]s) ? strpos(%[2]s, strval(%[1]s)) !== false : false))", l, r)
			} else if op == "+" {
				expr = fmt.Sprintf("((is_array(%[1]s) && is_array(%[2]s)) ? array_merge(%[1]s, %[2]s) : ((is_string(%[1]s) || is_string(%[2]s)) ? (%[1]s . %[2]s) : (%[1]s + %[2]s)))", l, r)
			} else if op == "/" {
				expr = fmt.Sprintf("((is_int(%[1]s) && is_int(%[2]s)) ? intdiv(%[1]s, %[2]s) : (%[1]s / %[2]s))", l, r)
			} else if op == "%" {
				expr = fmt.Sprintf("((is_int(%[1]s) && is_int(%[2]s)) ? (%[1]s %% %[2]s) : fmod(%[1]s, %[2]s))", l, r)
			} else if op == "union_all" {
				expr = fmt.Sprintf("array_merge(%s, %s)", l, r)
			} else if op == "union" {
				expr = fmt.Sprintf("array_values(array_unique(array_merge(%s, %s), SORT_REGULAR))", l, r)
			} else if op == "except" {
				expr = fmt.Sprintf("array_values(array_diff(%s, %s))", l, r)
			} else if op == "intersect" {
				expr = fmt.Sprintf("array_values(array_intersect(%s, %s))", l, r)
			} else {
				expr = fmt.Sprintf("(%s %s %s)", l, op, r)
			}
			operands[i] = expr
			operands = append(operands[:i+1], operands[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state in binary expr")
	}
	return operands[0], nil
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
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	// Special case: "contains" method on strings or lists.
	if len(p.Ops) == 1 && p.Ops[0].Call != nil && p.Target.Selector != nil &&
		len(p.Target.Selector.Tail) > 0 && p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "contains" {
		recvSel := &parser.SelectorExpr{Root: p.Target.Selector.Root, Tail: p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1]}
		recvExpr, err := c.compilePrimary(&parser.Primary{Selector: recvSel})
		if err != nil {
			return "", err
		}
		if len(p.Ops[0].Call.Args) != 1 {
			return "", fmt.Errorf("contains expects 1 arg")
		}
		arg, err := c.compileExpr(p.Ops[0].Call.Args[0])
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("(is_array(%[1]s) ? (array_key_exists(%[2]s, %[1]s) || in_array(%[2]s, %[1]s, true)) : (is_string(%[1]s) ? strpos(%[1]s, strval(%[2]s)) !== false : false))", recvExpr, arg)
		return expr, nil
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx := op.Index
			if idx.Colon == nil {
				key, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("%s[%s]", expr, key)
			} else {
				start := "0"
				if idx.Start != nil {
					s, err := c.compileExpr(idx.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("(is_array(%s) ? count(%s) : strlen(%s))", expr, expr, expr)
				if idx.End != nil {
					e, err := c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				length := fmt.Sprintf("(%s) - (%s)", end, start)
				expr = fmt.Sprintf("(is_array(%[1]s) ? array_slice(%[1]s, %[2]s, %[3]s) : substr(%[1]s, %[2]s, %[3]s))", expr, start, length)
			}
		} else if op.Call != nil {
			call, err := c.compileCallOp(expr, op.Call)
			if err != nil {
				return "", err
			}
			expr = call
		} else if op.Cast != nil {
			typ := types.ResolveTypeRef(op.Cast.Type, c.env)
			switch t := typ.(type) {
			case types.IntType, types.Int64Type:
				expr = fmt.Sprintf("(int)(%s)", expr)
			case types.FloatType:
				expr = fmt.Sprintf("(float)(%s)", expr)
			case types.StringType:
				expr = fmt.Sprintf("(string)(%s)", expr)
			case types.BoolType:
				expr = fmt.Sprintf("(bool)(%s)", expr)
			case types.StructType:
				if !c.typeNames[t.Name] {
					c.compileStructType(t)
				}
				expr = fmt.Sprintf("new %s((array)%s)", sanitizeName(t.Name), expr)
			case types.ListType:
				if st, ok := t.Elem.(types.StructType); ok {
					if !c.typeNames[st.Name] {
						c.compileStructType(st)
					}
					expr = fmt.Sprintf("array_map(fn($it) => new %s((array)$it), %s)", sanitizeName(st.Name), expr)
				}
			default:
				// ignore other types
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
		// Determine if the selector root refers to a map. If so emit
		// array indexing syntax instead of object fields.
		name := "$" + sanitizeName(p.Selector.Root)
		if c.methodFields != nil && c.methodFields[p.Selector.Root] {
			name = "$this->" + sanitizeName(p.Selector.Root)
		}
		// If we can't determine the type assume maps for dataset rows.
		isMap := true
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				switch t.(type) {
				case types.MapType:
					isMap = true
				case types.StructType, types.GroupType:
					isMap = false
					if _, ok := t.(types.GroupType); ok && len(p.Selector.Tail) == 0 {
						name += "->Items"
					}
				}
			}
		}
		if len(p.Selector.Tail) > 0 {
			if isMap {
				for _, f := range p.Selector.Tail {
					name += fmt.Sprintf("['%s']", f)
				}
			} else {
				name += "->" + strings.Join(p.Selector.Tail, "->")
			}
			return name, nil
		}
		if c.env != nil {
			if ut, ok := c.env.FindUnionByVariant(p.Selector.Root); ok {
				st := ut.Variants[p.Selector.Root]
				if len(st.Order) == 0 {
					if !c.typeNames[st.Name] {
						c.compileStructType(st)
					}
					return fmt.Sprintf("new %s()", sanitizeName(p.Selector.Root)), nil
				}
			}
		}
		return name, nil
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
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
		return c.compileMapLiteral(p.Map)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
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
	if fn, ok := c.env.GetFunc(name); ok && len(call.Args) < len(fn.Params) {
		missing := fn.Params[len(call.Args):]
		params := make([]string, len(missing))
		for i, p := range missing {
			params[i] = "$" + sanitizeName(p.Name)
		}
		callArgs := append(append([]string{}, args...), params...)
		callName := name
		if c.locals[name] {
			callName = "$" + sanitizeName(name)
		} else if c.funcs[name] {
			callName = funcPrefix + sanitizeName(name)
		}
		return fmt.Sprintf("function(%s) { return %s(%s); }", strings.Join(params, ", "), callName, strings.Join(callArgs, ", ")), nil
	}
	if c.locals[name] {
		return fmt.Sprintf("$%s(%s)", sanitizeName(name), strings.Join(args, ", ")), nil
	}
	if c.funcs[name] {
		return fmt.Sprintf("%s%s(%s)", funcPrefix, sanitizeName(name), strings.Join(args, ", ")), nil
	}
	switch name {
	case "print":
		if len(args) == 0 {
			return "", fmt.Errorf("print expects at least 1 arg")
		}
		joined := strings.Join(args, ", ")
		c.use("_print")
		return fmt.Sprintf("_print(%s)", joined), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("(is_array(%[1]s) ? count(%[1]s) : strlen(%[1]s))", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("strval(%s)", args[0]), nil
	case "input":
		if len(args) != 0 {
			return "", fmt.Errorf("input expects no args")
		}
		return "trim(fgets(STDIN))", nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		return fmt.Sprintf("echo json_encode(%s), PHP_EOL", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		return fmt.Sprintf("array_sum(%s)", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("(is_array(%[1]s) ? count(%[1]s) : strlen(%[1]s))", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(count(%[1]s) ? array_sum(%[1]s) / count(%[1]s) : 0)", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		return fmt.Sprintf("(count(%[1]s) ? max(%[1]s) : 0)", args[0]), nil
	case "concat":
		if len(args) == 0 {
			return "[]", nil
		}
		return fmt.Sprintf("array_merge(%s)", strings.Join(args, ", ")), nil
	case "reverse":
		if len(args) != 1 {
			return "", fmt.Errorf("reverse expects 1 arg")
		}
		return fmt.Sprintf("(is_array(%[1]s) ? array_reverse(%[1]s) : (is_string(%[1]s) ? strrev(%[1]s) : null))", args[0]), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("array_merge(%s, [%s])", args[0], args[1]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("array_values(%s)", args[0]), nil
	case "substr", "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substr expects 3 args")
		}
		return fmt.Sprintf("substr(%s, %s, %s - %s)", args[0], args[1], args[2], args[1]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		return fmt.Sprintf("((is_object(%[1]s) && property_exists(%[1]s, 'Items')) ? count(%[1]s->Items) : (is_array(%[1]s) ? count(%[1]s) : (is_string(%[1]s) ? strlen(%[1]s) : 0))) > 0", args[0]), nil
	default:
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		var k string
		if s, ok := types.SimpleStringKey(it.Key); ok {
			k = fmt.Sprintf("%q", s)
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
		items[i] = fmt.Sprintf("%s => %s", k, v)
	}
	return "[" + strings.Join(items, ", ") + "]", nil
}

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (string, error) {
	if st, ok := c.env.GetStruct(s.Name); ok {
		if !c.typeNames[st.Name] {
			c.compileStructType(st)
		}
	}
	items := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("'%s' => %s", f.Name, v)
	}
	return fmt.Sprintf("new %s([%s])", sanitizeName(s.Name), strings.Join(items, ", ")), nil
}

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	if c.typeNames[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("class %s {", name))
	c.indent++
	fields := make([]string, len(st.Order))
	for i, fn := range st.Order {
		fields[i] = fn
		c.writeln(fmt.Sprintf("public $%s;", sanitizeName(fn)))
	}
	c.writeln("public function __construct($fields = []) {")
	c.indent++
	for _, f := range fields {
		c.writeln(fmt.Sprintf("$this->%s = $fields['%s'] ?? null;", sanitizeName(f), f))
	}
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("}")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Skip == nil && q.Take == nil {
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		c.env = child
		var cond string
		if q.Where != nil {
			var err error
			cond, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		var sortExpr string
		if q.Sort != nil {
			s, err := c.compileExpr(q.Sort)
			if err != nil {
				c.env = orig
				return "", err
			}
			sortExpr = s
		}
		capture := queryFreeVars(q, orig)
		c.env = orig

		use := ""
		if len(capture) > 0 {
			use = " use (" + strings.Join(capture, ", ") + ")"
		}
		var b strings.Builder
		b.WriteString("(function()" + use + " {\n")
		b.WriteString(fmt.Sprintf("\t$_src = (is_string(%[1]s) ? str_split(%[1]s) : %[1]s);\n", src))
		if cond != "" {
			b.WriteString(fmt.Sprintf("\t$_src = array_values(array_filter($_src, function($%s)%s { return (%s); }));\n", sanitizeName(q.Var), use, cond))
		}
		b.WriteString(fmt.Sprintf("\t$_groups = _group_by($_src, function($%s)%s { return %s; });\n", sanitizeName(q.Var), use, keyExpr))
		if sortExpr != "" {
			b.WriteString("\t$pairs = [];\n")
			b.WriteString(fmt.Sprintf("\tforeach ($_groups as $%s) {\n", sanitizeName(q.Group.Name)))
			b.WriteString(fmt.Sprintf("\t\t$pairs[] = ['item' => $%s, 'key' => %s];\n", sanitizeName(q.Group.Name), sortExpr))
			b.WriteString("\t}\n")
			b.WriteString("\tusort($pairs, function($a, $b) {\n")
			b.WriteString("\t\t$ak = $a['key']; $bk = $b['key'];\n")
			b.WriteString("\t\tif (is_int($ak) && is_int($bk)) return $ak <=> $bk;\n")
			b.WriteString("\t\tif (is_string($ak) && is_string($bk)) return $ak <=> $bk;\n")
			b.WriteString("\t\treturn strcmp(strval($ak), strval($bk));\n")
			b.WriteString("\t});\n")
			b.WriteString("\t$_groups = array_map(fn($p) => $p['item'], $pairs);\n")
		}
		b.WriteString("\t$res = [];\n")
		b.WriteString(fmt.Sprintf("\tforeach ($_groups as $%s) {\n", sanitizeName(q.Group.Name)))
		b.WriteString(fmt.Sprintf("\t\t$res[] = %s;\n", valExpr))
		b.WriteString("\t}\n")
		b.WriteString("\treturn $res;\n")
		b.WriteString("})()")
		c.use("_group")
		c.use("_group_by")
		return b.String(), nil
	}

	// Fast path for simple queries without joins or sorting
	if len(q.Joins) == 0 && q.Sort == nil {
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		for _, f := range q.Froms {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
		c.env = child
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		if arg, ok := isSumCall(q.Select); ok && q.Group == nil && q.Skip == nil && q.Take == nil {
			sumExpr, err := c.compileExpr(arg)
			if err != nil {
				c.env = orig
				return "", err
			}
			var cond string
			var outerCond string
			fromConds := make([]string, len(q.Froms))
			if q.Where != nil {
				cond, err = c.compileExpr(q.Where)
				if err != nil {
					c.env = orig
					return "", err
				}
				vars := exprVarSet(q.Where)
				if len(vars) == 1 {
					if vars[q.Var] {
						outerCond = cond
						cond = ""
					} else {
						for i, f := range q.Froms {
							if vars[f.Var] {
								fromConds[i] = cond
								cond = ""
								break
							}
						}
					}
				}
			}
			fromSrcs := make([]string, len(q.Froms))
			for i, f := range q.Froms {
				fs, err := c.compileExpr(f.Src)
				if err != nil {
					c.env = orig
					return "", err
				}
				fromSrcs[i] = fs
			}
			capture := queryFreeVars(q, orig)
			c.env = orig

			var b strings.Builder
			use := ""
			if len(capture) > 0 {
				use = " use (" + strings.Join(capture, ", ") + ")"
			}
			b.WriteString("(function()" + use + " {\n")
			b.WriteString("\t$sum = 0;\n")
			b.WriteString(fmt.Sprintf("\tforeach ((is_string(%[1]s) ? str_split(%[1]s) : %[1]s) as $%s) {\n", src, sanitizeName(q.Var)))
			if outerCond != "" {
				b.WriteString("\t\tif (!(" + outerCond + ")) { continue; }\n")
			}
			indent := "\t\t"
			for i, fs := range fromSrcs {
				b.WriteString(fmt.Sprintf(indent+"foreach ((is_string(%[1]s) ? str_split(%[1]s) : %[1]s) as $%s) {\n", fs, sanitizeName(q.Froms[i].Var)))
				indent += "\t"
				if fromConds[i] != "" {
					b.WriteString(indent + "if (!(" + fromConds[i] + ")) { continue; }\n")
				}
			}
			if cond != "" {
				b.WriteString(indent + "if (" + cond + ") {\n")
				indent += "\t"
			}
			b.WriteString(indent + "$sum += " + sumExpr + ";\n")
			if cond != "" {
				indent = indent[:len(indent)-1]
				b.WriteString(indent + "}\n")
			}
			for range fromSrcs {
				indent = indent[:len(indent)-1]
				b.WriteString(indent + "}\n")
			}
			b.WriteString("\t}\n")
			b.WriteString("\treturn $sum;\n")
			b.WriteString("})()")
			return b.String(), nil
		}
		var cond string
		var skipExpr string
		var takeExpr string
		var outerCond string
		fromConds := make([]string, len(q.Froms))
		if q.Where != nil {
			cond, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
			vars := exprVarSet(q.Where)
			if len(vars) == 1 {
				if vars[q.Var] {
					outerCond = cond
					cond = ""
				} else {
					for i, f := range q.Froms {
						if vars[f.Var] {
							fromConds[i] = cond
							cond = ""
							break
						}
					}
				}
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
		fromSrcs := make([]string, len(q.Froms))
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			fromSrcs[i] = fs
		}
		capture := queryFreeVars(q, orig)
		c.env = orig

		var b strings.Builder
		use := ""
		if len(capture) > 0 {
			use = " use (" + strings.Join(capture, ", ") + ")"
		}
		b.WriteString("(function()" + use + " {\n")
		b.WriteString("\t$res = [];\n")
		b.WriteString(fmt.Sprintf("\tforeach ((is_string(%[1]s) ? str_split(%[1]s) : %[1]s) as $%s) {\n", src, sanitizeName(q.Var)))
		if outerCond != "" {
			b.WriteString("\t\tif (!(" + outerCond + ")) { continue; }\n")
		}
		indent := "\t\t"
		for i, fs := range fromSrcs {
			b.WriteString(fmt.Sprintf(indent+"foreach ((is_string(%[1]s) ? str_split(%[1]s) : %[1]s) as $%s) {\n", fs, sanitizeName(q.Froms[i].Var)))
			indent += "\t"
			if fromConds[i] != "" {
				b.WriteString(indent + "if (!(" + fromConds[i] + ")) { continue; }\n")
			}
		}
		if cond != "" {
			b.WriteString(indent + "if (" + cond + ") {\n")
			indent += "\t"
		}
		b.WriteString(indent + "$res[] = " + sel + ";\n")
		if cond != "" {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "}\n")
		}
		for range fromSrcs {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "}\n")
		}
		b.WriteString("\t}\n")
		if skipExpr != "" {
			b.WriteString(fmt.Sprintf("\t$res = array_slice($res, %s);\n", skipExpr))
		}
		if takeExpr != "" {
			b.WriteString(fmt.Sprintf("\t$res = array_slice($res, 0, %s);\n", takeExpr))
		}
		b.WriteString("\treturn $res;\n")
		b.WriteString("})()")
		return b.String(), nil
	}

	// General path using _query helper
	child := types.NewEnv(c.env)
	varNames := []string{sanitizeName(q.Var)}
	child.SetVar(q.Var, types.AnyType{}, true)
	for _, f := range q.Froms {
		child.SetVar(f.Var, types.AnyType{}, true)
	}
	for _, j := range q.Joins {
		child.SetVar(j.Var, types.AnyType{}, true)
	}
	orig := c.env
	c.env = child
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		fromSrcs[i] = fs
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
	var whereExpr, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		whereExpr, err = c.compileExpr(q.Where)
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
	capture := queryFreeVars(q, orig)
	c.env = orig

	joins := make([]string, 0, len(q.Froms)+len(q.Joins))
	params := []string{sanitizeName(q.Var)}
	for i, fs := range fromSrcs {
		joins = append(joins, fmt.Sprintf("[ 'items' => %s ]", fs))
		params = append(params, sanitizeName(q.Froms[i].Var))
	}
	paramCopy = append([]string(nil), params...)
	for i, js := range joinSrcs {
		onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
		spec := fmt.Sprintf("[ 'items' => %s, 'on' => function(%s)%s { return %s; }", js, "$"+strings.Join(onParams, ", $"), " use ("+strings.Join(capture, ", ")+")", joinOns[i])
		if q.Joins[i].Side != nil {
			side := *q.Joins[i].Side
			if side == "left" || side == "outer" {
				spec += ", 'left' => true"
			}
			if side == "right" || side == "outer" {
				spec += ", 'right' => true"
			}
		}
		spec += " ]"
		joins = append(joins, spec)
		paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
	}

	allParams := "$" + strings.Join(paramCopy, ", $")
	use := ""
	if len(capture) > 0 {
		use = " use (" + strings.Join(capture, ", ") + ")"
	}
	selectFn := fmt.Sprintf("function(%s)%s { return %s; }", allParams, use, val)
	var whereFn, sortFn string
	if whereExpr != "" {
		whereFn = fmt.Sprintf("function(%s)%s { return (%s); }", allParams, use, whereExpr)
	}
	if sortExpr != "" {
		sortFn = fmt.Sprintf("function(%s)%s { return (%s); }", allParams, use, sortExpr)
	}

	var b strings.Builder
	b.WriteString("(function()" + use + " {\n")
	if whereExpr != "" {
		vars := exprVarSet(q.Where)
		if len(vars) == 1 && vars[q.Var] {
			filterFn := fmt.Sprintf("function($%s)%s { return (%s); }", sanitizeName(q.Var), use, whereExpr)
			b.WriteString(fmt.Sprintf("\t$_src = array_values(array_filter((is_string(%[1]s) ? str_split(%[1]s) : %[1]s), %s));\n", src, filterFn))
			whereExpr = ""
		} else {
			b.WriteString("\t$_src = " + src + ";\n")
		}
	} else {
		b.WriteString("\t$_src = " + src + ";\n")
	}
	b.WriteString("\treturn _query($_src, [\n")
	for i, j := range joins {
		b.WriteString("\t\t" + j)
		if i != len(joins)-1 {
			b.WriteString(",")
		}
		b.WriteString("\n")
	}
	b.WriteString("\t], [ 'select' => " + selectFn)
	if whereFn != "" {
		b.WriteString(", 'where' => " + whereFn)
	}
	if sortFn != "" {
		b.WriteString(", 'sortKey' => " + sortFn)
	}
	if skipExpr != "" {
		b.WriteString(", 'skip' => " + skipExpr)
	}
	if takeExpr != "" {
		b.WriteString(", 'take' => " + takeExpr)
	}
	b.WriteString(" ]);\n")
	b.WriteString("})()")
	c.use("_query")
	return b.String(), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int), nil
	case l.Float != nil:
		f := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(f, ".eE") {
			f += ".0"
		}
		return f, nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Null:
		return "null", nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	}
	return "", fmt.Errorf("unknown literal")
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = "$" + sanitizeName(p.Name)
	}
	subEnv := types.NewEnv(c.env)
	for _, p := range fn.Params {
		var pt types.Type = types.AnyType{}
		if p.Type != nil {
			pt = types.ResolveTypeRef(p.Type, c.env)
		}
		subEnv.SetVar(p.Name, pt, true)
	}
	sub := &Compiler{env: subEnv}
	var body bytes.Buffer
	sub.buf = body
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr + ";")
	} else {
		for _, st := range fn.BlockBody {
			if err := sub.compileStmt(st); err != nil {
				return "", err
			}
		}
	}
	captured := freeVars(fn, params)
	use := ""
	if len(captured) > 0 {
		use = " use (" + strings.Join(captured, ", ") + ")"
	}
	bodyStr := indentBlock(sub.buf.String(), 1)
	return fmt.Sprintf("function (%s)%s {\n%s}", strings.Join(params, ", "), use, bodyStr), nil
}

func indentBlock(s string, indent int) string {
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	pref := strings.Repeat("\t", indent)
	for i, l := range lines {
		lines[i] = pref + l
	}
	return strings.Join(lines, "\n") + "\n"
}

func freeVars(fn *parser.FunExpr, params []string) []string {
	vars := map[string]struct{}{}
	scanExpr(fn.ExprBody, vars)
	for _, st := range fn.BlockBody {
		scanStmt(st, vars)
	}
	m := make(map[string]struct{})
	for v := range vars {
		skip := false
		for _, p := range params {
			if p == "$"+sanitizeName(v) {
				skip = true
				break
			}
		}
		if !skip {
			m["$"+sanitizeName(v)] = struct{}{}
		}
	}
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

func scanStmt(s *parser.Statement, vars map[string]struct{}) {
	switch {
	case s.Let != nil:
		scanExpr(s.Let.Value, vars)
	case s.Var != nil:
		scanExpr(s.Var.Value, vars)
	case s.Expect != nil:
		scanExpr(s.Expect.Value, vars)
	case s.Test != nil:
		for _, st := range s.Test.Body {
			scanStmt(st, vars)
		}
	case s.Assign != nil:
		scanExpr(s.Assign.Value, vars)
	case s.Return != nil:
		scanExpr(s.Return.Value, vars)
	case s.Expr != nil:
		scanExpr(s.Expr.Expr, vars)
	case s.For != nil:
		scanExpr(s.For.Source, vars)
		scanExpr(s.For.RangeEnd, vars)
		for _, st := range s.For.Body {
			scanStmt(st, vars)
		}
	case s.While != nil:
		scanExpr(s.While.Cond, vars)
		for _, st := range s.While.Body {
			scanStmt(st, vars)
		}
	case s.If != nil:
		scanExpr(s.If.Cond, vars)
		for _, st := range s.If.Then {
			scanStmt(st, vars)
		}
		if s.If.ElseIf != nil {
			scanStmt(&parser.Statement{If: s.If.ElseIf}, vars)
		}
		for _, st := range s.If.Else {
			scanStmt(st, vars)
		}
	}
}

func scanExpr(e *parser.Expr, vars map[string]struct{}) {
	if e == nil {
		return
	}
	scanUnary(e.Binary.Left, vars)
	for _, op := range e.Binary.Right {
		scanPostfix(op.Right, vars)
	}
}

func scanUnary(u *parser.Unary, vars map[string]struct{}) {
	if u == nil {
		return
	}
	scanPostfix(u.Value, vars)
}

func scanPostfix(p *parser.PostfixExpr, vars map[string]struct{}) {
	if p == nil {
		return
	}
	scanPrimary(p.Target, vars)
	for _, op := range p.Ops {
		if op.Index != nil {
			scanExpr(op.Index.Start, vars)
			scanExpr(op.Index.End, vars)
		}
		if op.Call != nil {
			for _, a := range op.Call.Args {
				scanExpr(a, vars)
			}
		}
	}
}

func queryFreeVars(q *parser.QueryExpr, env *types.Env) []string {
	vars := map[string]struct{}{}
	scanExpr(q.Source, vars)
	for _, f := range q.Froms {
		scanExpr(f.Src, vars)
	}
	for _, j := range q.Joins {
		scanExpr(j.Src, vars)
		scanExpr(j.On, vars)
	}
	if q.Group != nil {
		scanExpr(q.Group.Exprs[0], vars)
	}
	scanExpr(q.Select, vars)
	scanExpr(q.Where, vars)
	scanExpr(q.Sort, vars)
	scanExpr(q.Skip, vars)
	scanExpr(q.Take, vars)
	delete(vars, q.Var)
	for _, f := range q.Froms {
		delete(vars, f.Var)
	}
	for _, j := range q.Joins {
		delete(vars, j.Var)
	}
	if q.Group != nil {
		delete(vars, q.Group.Name)
	}
	outMap := map[string]struct{}{}
	for k := range vars {
		if env != nil {
			if _, err := env.GetVar(k); err != nil {
				continue
			}
		}
		outMap["$"+sanitizeName(k)] = struct{}{}
	}
	out := make([]string, 0, len(outMap))
	for k := range outMap {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

func scanPrimary(p *parser.Primary, vars map[string]struct{}) {
	if p == nil {
		return
	}
	if p.Selector != nil {
		vars[p.Selector.Root] = struct{}{}
	}
	if p.Group != nil {
		scanExpr(p.Group, vars)
	}
	if p.FunExpr != nil {
		scanExpr(p.FunExpr.ExprBody, vars)
		for _, st := range p.FunExpr.BlockBody {
			scanStmt(st, vars)
		}
	}
	if p.List != nil {
		for _, e := range p.List.Elems {
			scanExpr(e, vars)
		}
	}
	if p.Map != nil {
		for _, it := range p.Map.Items {
			if _, ok := types.SimpleStringKey(it.Key); !ok {
				scanExpr(it.Key, vars)
			}
			scanExpr(it.Value, vars)
		}
	}
	if p.Call != nil {
		for _, a := range p.Call.Args {
			scanExpr(a, vars)
		}
	}
}

func mutatedVars(stmts []*parser.Statement) map[string]bool {
	vars := map[string]bool{}
	var walk func(*parser.Statement)
	walk = func(s *parser.Statement) {
		switch {
		case s.Assign != nil:
			vars[s.Assign.Name] = true
		case s.For != nil:
			for _, st := range s.For.Body {
				walk(st)
			}
		case s.While != nil:
			for _, st := range s.While.Body {
				walk(st)
			}
		case s.If != nil:
			for _, st := range s.If.Then {
				walk(st)
			}
			if s.If.ElseIf != nil {
				walk(&parser.Statement{If: s.If.ElseIf})
			}
			for _, st := range s.If.Else {
				walk(st)
			}
		}
	}
	for _, st := range stmts {
		walk(st)
	}
	return vars
}

func isComposite(t types.Type) bool {
	switch t.(type) {
	case types.ListType, types.MapType, types.StructType, types.UnionType:
		return true
	default:
		return false
	}
}

func isCompositeParam(p *parser.Param) bool {
	if p == nil || p.Type == nil {
		return false
	}
	if p.Type.Generic != nil {
		switch p.Type.Generic.Name {
		case "list", "map":
			return true
		}
	}
	return false
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		p := *l.Path
		if c.baseDir != "" && !strings.HasPrefix(p, "/") {
			p = filepath.Join(c.baseDir, p)
		}
		path = fmt.Sprintf("%q", p)
	}
	fn := "_load_json"
	if l.With != nil {
		v := l.With.Binary.Left.Value
		if len(v.Ops) == 0 && v.Target.Map != nil {
			for _, it := range v.Target.Map.Items {
				key, ok := types.SimpleStringKey(it.Key)
				if !ok {
					continue
				}
				if key == "format" && it.Value != nil {
					mv := it.Value.Binary.Left.Value
					if len(mv.Ops) == 0 && mv.Target.Lit != nil && mv.Target.Lit.Str != nil {
						if *mv.Target.Lit.Str == "yaml" {
							fn = "_load_yaml"
						}
					}
				}
			}
		}
	}
	c.use(fn)
	return fmt.Sprintf("%s(%s)", fn, path), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	c.use("_save_json")
	return fmt.Sprintf("_save_json(%s, %s)", src, path), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	urlStr, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	var withStr string
	if f.With != nil {
		w, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		withStr = w
	} else {
		withStr = "null"
	}
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", urlStr, withStr), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var prompt, text, model string
	params := []string{}
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
			params = append(params, fmt.Sprintf("'%s' => %s", f.Name, v))
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
		paramStr = "[" + strings.Join(params, ", ") + "]"
	}
	if model == "" {
		model = "null"
	}
	if g.Target == "embedding" {
		c.use("_gen_embed")
		return fmt.Sprintf("_gen_embed(%s, %s, %s)", text, model, paramStr), nil
	}
	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_gen_struct")
			return fmt.Sprintf("_gen_struct(%s, %s, %s)", prompt, model, paramStr), nil
		}
	}
	c.use("_gen_text")
	return fmt.Sprintf("_gen_text(%s, %s, %s)", prompt, model, paramStr), nil
}

func exprVarSet(e *parser.Expr) map[string]bool {
	if e == nil {
		return map[string]bool{}
	}
	vars := map[string]struct{}{}
	scanExpr(e, vars)
	res := make(map[string]bool, len(vars))
	for v := range vars {
		res[v] = true
	}
	return res
}

func isSumCall(e *parser.Expr) (*parser.Expr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	call := p.Target.Call
	if call.Func != "sum" || len(call.Args) != 1 {
		return nil, false
	}
	return call.Args[0], true
}
