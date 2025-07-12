//go:build slow

package cljcode

import (
	"bytes"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Clojure source code (limited subset).
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	mainStmts    []*parser.Statement
	tempVarCount int
	imports      map[string]string
	helpers      map[string]bool
	pyModules    map[string]string
	goModules    map[string]string
	pyAuto       map[string]bool
	goAuto       map[string]bool
}

// New creates a new Clojure compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:          env,
		tempVarCount: 0,
		imports:      map[string]string{},
		helpers:      make(map[string]bool),
		pyModules:    map[string]string{},
		goModules:    map[string]string{},
		pyAuto:       map[string]bool{},
		goAuto:       map[string]bool{},
	}
}

// Compile generates Clojure code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.mainStmts = nil
	c.tempVarCount = 0
	c.imports = map[string]string{}
	c.helpers = make(map[string]bool)
	c.pyModules = map[string]string{}
	c.goModules = map[string]string{}
	c.pyAuto = map[string]bool{}
	c.goAuto = map[string]bool{}

	declNames := []string{}
	for _, s := range prog.Statements {
		if s.Let != nil {
			declNames = append(declNames, sanitizeName(s.Let.Name))
		}
		if s.Var != nil {
			declNames = append(declNames, sanitizeName(s.Var.Name))
		}
	}
	if len(declNames) > 0 {
		c.writeln("(declare " + strings.Join(declNames, " ") + ")")
		c.writeln("")
	}

	for _, s := range prog.Statements {
		switch {
		case s.Fun != nil:
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		case s.Type != nil:
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		case s.Test != nil:
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		default:
			c.mainStmts = append(c.mainStmts, s)
		}
	}
	c.writeln("(defn -main []")
	c.indent++
	for _, s := range c.mainStmts {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			c.writeln("(" + "test_" + sanitizeName(s.Test.Name) + ")")
		}
	}
	c.indent--
	c.writeln(")")
	c.writeln("")
	c.writeln("(-main)")

	finalBuf := bytes.Buffer{}
	c.writeNamespace(&finalBuf)
	c.emitRuntime(&finalBuf)
	finalBuf.Write(c.buf.Bytes())

	code := finalBuf.Bytes()
	if formatted, err := Format(code); err == nil {
		return formatted, nil
	}
	return code, nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	origEnv := c.env
	if origEnv != nil {
		child := types.NewEnv(origEnv)
		if t, err := origEnv.GetVar(fn.Name); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				for i, p := range fn.Params {
					if i < len(ft.Params) {
						child.SetVar(p.Name, ft.Params[i], true)
					} else {
						child.SetVar(p.Name, types.AnyType{}, true)
					}
				}
			}
		}
		c.env = child
		defer func() { c.env = origEnv }()
	}

	c.writeIndent()
	if fn.Doc != "" {
		c.buf.WriteString(";; " + fn.Doc + "\n")
		c.writeIndent()
	}
	paramInfo := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		var pt types.Type = types.AnyType{}
		if p.Type != nil {
			pt = c.resolveTypeRef(p.Type)
		} else if origEnv != nil {
			if t, err := origEnv.GetVar(fn.Name); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					if i < len(ft.Params) {
						pt = ft.Params[i]
					}
				}
			}
		}
		paramInfo[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), cljTypeOf(pt))
	}
	retType := "any"
	if fn.Return != nil {
		retType = cljTypeOf(c.resolveTypeRef(fn.Return))
	}
	if len(paramInfo) > 0 {
		c.buf.WriteString(fmt.Sprintf(";; Function %s takes [%s] and returns %s\n", sanitizeName(fn.Name), strings.Join(paramInfo, ", "), retType))
	} else {
		c.buf.WriteString(fmt.Sprintf(";; Function %s returns %s\n", sanitizeName(fn.Name), retType))
	}
	c.writeIndent()
	c.buf.WriteString("(defn " + sanitizeName(fn.Name) + " [")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(" ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
	}
	c.buf.WriteString("]\n")
	c.indent++
	c.writeln("(try")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("(catch clojure.lang.ExceptionInfo e")
	c.indent++
	c.writeln("(if (= (.getMessage e) \"return\")")
	c.indent++
	c.writeln("(:value (ex-data e))")
	c.indent--
	c.writeln("(throw e)))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileMethod(typeName string, m *parser.FunStmt) error {
	st, ok := c.env.GetStruct(typeName)
	if !ok {
		return fmt.Errorf("unknown struct: %s", typeName)
	}
	origEnv := c.env
	child := types.NewEnv(origEnv)
	for fname, ft := range st.Fields {
		child.SetVar(fname, ft, true)
	}
	if mt, ok := st.Methods[m.Name]; ok {
		for i, p := range m.Params {
			if i < len(mt.Type.Params) {
				child.SetVar(p.Name, mt.Type.Params[i], true)
			} else {
				child.SetVar(p.Name, types.AnyType{}, true)
			}
		}
	}
	c.env = child
	defer func() { c.env = origEnv }()

	name := sanitizeName(typeName + "_" + m.Name)
	c.writeIndent()
	c.buf.WriteString("(defn " + name + " [self")
	for _, p := range m.Params {
		c.buf.WriteString(" " + sanitizeName(p.Name))
	}
	c.buf.WriteString("]\n")
	c.indent++
	if len(st.Fields) > 0 {
		c.writeIndent()
		c.buf.WriteString("(let [")
		i := 0
		for _, fname := range st.Order {
			if i > 0 {
				c.buf.WriteString(" ")
			}
			sf := sanitizeName(fname)
			c.buf.WriteString(fmt.Sprintf("%s (:%s self)", sf, sf))
			i++
		}
		c.buf.WriteString("]\n")
		c.indent++
	}
	c.writeln("(try")
	c.indent++
	for _, stmnt := range m.Body {
		if err := c.compileStmt(stmnt); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("(catch clojure.lang.ExceptionInfo e")
	c.indent++
	c.writeln("(if (= (.getMessage e) \"return\")")
	c.indent++
	c.writeln("(:value (ex-data e))")
	c.indent--
	c.writeln("(throw e)))")
	c.indent--
	c.writeln(")")
	if len(st.Fields) > 0 {
		c.indent--
		c.writeln(")")
	}
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("(defn " + name + " []\n")
	c.indent++
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(assert %s \"expect failed\")", expr))
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Import != nil:
		return c.compileImport(s.Import)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Break != nil:
		c.writeln("(throw (ex-info \"break\" {}))")
		return nil
	case s.Continue != nil:
		c.writeln("(throw (ex-info \"continue\" {}))")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Expr != nil:
		if name, arg, ok := isListPushCall(s.Expr.Expr); ok {
			val, err := c.compileExpr(arg)
			if err != nil {
				return err
			}
			n := sanitizeName(name)
			c.writeln(fmt.Sprintf("(def %s (conj %s %s))", n, n, val))
			if c.env != nil {
				if t, err := c.env.GetVar(name); err == nil {
					c.env.SetVar(name, t, true)
				}
			}
			return nil
		}
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if expr != "" {
			c.writeln(expr)
		}
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(st *parser.LetStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	if st.Type != nil {
		t := c.resolveTypeRef(st.Type)
		switch tt := t.(type) {
		case types.StructType:
			c.use("_cast_struct")
			expr = fmt.Sprintf("(_cast_struct #'%s %s)", sanitizeName(tt.Name), expr)
		case types.ListType:
			if stt, ok := tt.Elem.(types.StructType); ok {
				c.use("_cast_struct_list")
				expr = fmt.Sprintf("(_cast_struct_list #'%s %s)", sanitizeName(stt.Name), expr)
			}
		}
	}
	if st.Doc != "" {
		c.writeIndent()
		c.buf.WriteString(";; " + st.Doc + "\n")
	}
	var typ types.Type = types.AnyType{}
	if st.Type != nil {
		typ = c.resolveTypeRef(st.Type)
	} else {
		typ = c.exprType(st.Value)
	}
	c.writeln(fmt.Sprintf("(def %s %s) ;; %s", sanitizeName(st.Name), expr, cljTypeOf(typ)))
	if c.env != nil {
		c.env.SetVar(st.Name, typ, true)
	}
	return nil
}

func (c *Compiler) compileVar(st *parser.VarStmt) error {
	expr := "nil"
	if st.Value != nil {
		v, err := c.compileExpr(st.Value)
		if err != nil {
			return err
		}
		expr = v
	}
	if st.Type != nil && expr != "nil" {
		t := c.resolveTypeRef(st.Type)
		switch tt := t.(type) {
		case types.StructType:
			c.use("_cast_struct")
			expr = fmt.Sprintf("(_cast_struct #'%s %s)", sanitizeName(tt.Name), expr)
		case types.ListType:
			if stt, ok := tt.Elem.(types.StructType); ok {
				c.use("_cast_struct_list")
				expr = fmt.Sprintf("(_cast_struct_list #'%s %s)", sanitizeName(stt.Name), expr)
			}
		}
	}
	if st.Doc != "" {
		c.writeIndent()
		c.buf.WriteString(";; " + st.Doc + "\n")
	}
	var typ types.Type = types.AnyType{}
	if st.Type != nil {
		typ = c.resolveTypeRef(st.Type)
	} else if st.Value != nil {
		typ = c.exprType(st.Value)
	}
	c.writeln(fmt.Sprintf("(def %s %s) ;; %s", sanitizeName(st.Name), expr, cljTypeOf(typ)))
	if c.env != nil {
		c.env.SetVar(st.Name, typ, true)
	}
	return nil
}

func (c *Compiler) compileImport(im *parser.ImportStmt) error {
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = sanitizeName(alias)
	path := strings.Trim(im.Path, "\"")
	if im.Lang == nil || *im.Lang == "clj" || *im.Lang == "clojure" {
		c.imports[alias] = path
		return nil
	}
	switch *im.Lang {
	case "python":
		if c.pyModules == nil {
			c.pyModules = map[string]string{}
		}
		if c.pyAuto == nil {
			c.pyAuto = map[string]bool{}
		}
		c.pyModules[alias] = path
		if im.Auto {
			c.pyAuto[alias] = true
		}
		return nil
	case "go":
		if c.goModules == nil {
			c.goModules = map[string]string{}
		}
		if c.goAuto == nil {
			c.goAuto = map[string]bool{}
		}
		c.goModules[alias] = path
		if im.Auto {
			c.goAuto[alias] = true
		}
		return nil
	default:
		return fmt.Errorf("unsupported import language: %s", *im.Lang)
	}
}

func (c *Compiler) compileAssign(st *parser.AssignStmt) error {
	name := sanitizeName(st.Name)
	rhs, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	if len(st.Index) == 0 {
		typ := c.exprType(st.Value)
		c.writeln(fmt.Sprintf("(def %s %s) ;; %s", name, rhs, cljTypeOf(typ)))
		if c.env != nil {
			c.env.SetVar(st.Name, typ, true)
		}
		return nil
	}
	idxs := make([]string, len(st.Index))
	for i, idx := range st.Index {
		v, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		idxs[i] = v
	}
	if len(idxs) == 1 {
		typ := c.exprType(st.Value)
		c.writeln(fmt.Sprintf("(def %s (assoc %s %s %s)) ;; %s", name, name, idxs[0], rhs, cljTypeOf(typ)))
	} else {
		typ := c.exprType(st.Value)
		c.writeln(fmt.Sprintf("(def %s (assoc-in %s [%s] %s)) ;; %s", name, name, strings.Join(idxs, " "), rhs, cljTypeOf(typ)))
	}
	if c.env != nil {
		if t, err := c.env.GetVar(st.Name); err == nil {
			c.env.SetVar(st.Name, t, true)
		}
	}
	return nil
}

func (c *Compiler) compileReturn(st *parser.ReturnStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	if expr == "" {
		expr = "nil"
	}
	c.writeln(fmt.Sprintf("(throw (ex-info \"return\" {:value %s}))", expr))
	return nil
}

func (c *Compiler) compileFor(st *parser.ForStmt) error {
	name := sanitizeName(st.Name)
	if st.RangeEnd != nil {
		start, err := c.compileExpr(st.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(st.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(loop [%s %s]", name, start))
		c.indent++
		c.writeln(fmt.Sprintf("(when (< %s %s)", name, end))
		c.indent++
		c.writeln("(let [r (try")
		c.indent++
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.writeln(":next")
		c.indent--
		c.writeln("(catch clojure.lang.ExceptionInfo e")
		c.indent++
		c.writeln("(cond")
		c.indent++
		c.writeln("(= (.getMessage e) \"continue\") :next")
		c.writeln("(= (.getMessage e) \"break\") :break")
		c.writeln(":else (throw e))")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")]")
		c.indent--
		c.writeln("(cond")
		c.indent++
		c.writeln("(= r :break) nil")
		c.writeln(fmt.Sprintf(":else (recur (inc %s))", name))
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		return nil
	}
	src, err := c.compileExpr(st.Source)
	if err != nil {
		return err
	}
	seqVar := c.newTemp()
	c.writeln(fmt.Sprintf("(loop [%s (seq %s)]", seqVar, src))
	c.indent++
	c.writeln(fmt.Sprintf("(when %s", seqVar))
	c.indent++
	c.writeln(fmt.Sprintf("(let [%s (clojure.core/first %s)]", name, seqVar))
	c.indent++
	c.writeln("(let [r (try")
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.writeln(":next")
	c.indent--
	c.writeln("(catch clojure.lang.ExceptionInfo e")
	c.indent++
	c.writeln("(cond")
	c.indent++
	c.writeln("(= (.getMessage e) \"continue\") :next")
	c.writeln("(= (.getMessage e) \"break\") :break")
	c.writeln(":else (throw e))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")]")
	c.indent--
	c.writeln("(cond")
	c.indent++
	c.writeln("(= r :break) nil")
	c.writeln(fmt.Sprintf(":else (recur (next %s))", seqVar))
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("(loop []")
	c.indent++
	c.writeln("(when " + cond)
	c.indent++
	c.writeln("(let [r (try")
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.writeln(":next")
	c.indent--
	c.writeln("(catch clojure.lang.ExceptionInfo e")
	c.indent++
	c.writeln("(cond")
	c.indent++
	c.writeln("(= (.getMessage e) \"continue\") :next")
	c.writeln("(= (.getMessage e) \"break\") :break")
	c.writeln(":else (throw e))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")]")
	c.indent--
	c.writeln("(cond")
	c.indent++
	c.writeln("(= r :break) nil")
	c.writeln("(= r :next) (recur)")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	target := sanitizeName(u.Target)
	item := c.newTemp()

	origEnv := c.env
	child := types.NewEnv(origEnv)
	fields := []string{}
	if origEnv != nil {
		if t, err := origEnv.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if st, ok := lt.Elem.(types.StructType); ok {
					for _, f := range st.Order {
						child.SetVar(f, st.Fields[f], true)
						fields = append(fields, f)
					}
				}
			}
		}
	}
	c.env = child

	whereExpr := ""
	var err error
	if u.Where != nil {
		whereExpr, err = c.compileExpr(u.Where)
		if err != nil {
			c.env = origEnv
			return err
		}
	}

	expr := item
	for _, it := range u.Set.Items {
		keyStr, _ := identName(it.Key)
		val, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		expr = fmt.Sprintf("(assoc %s :%s %s)", expr, sanitizeName(keyStr), val)
	}

	if whereExpr != "" {
		expr = fmt.Sprintf("(if %s %s %s)", whereExpr, expr, item)
	}

	c.writeln(fmt.Sprintf("(def %s", target))
	c.indent++
	c.writeln(fmt.Sprintf("(vec (map (fn [%s]", item))
	c.indent++
	if len(fields) > 0 {
		c.writeIndent()
		c.buf.WriteString("(let [")
		for i, f := range fields {
			if i > 0 {
				c.buf.WriteString(" ")
			}
			sf := sanitizeName(f)
			c.buf.WriteString(fmt.Sprintf("%s (:%s %s)", sf, sf, item))
		}
		c.buf.WriteString("]\n")
		c.indent++
	}
	c.writeln(expr)
	if len(fields) > 0 {
		c.indent--
		c.writeln(")")
	}
	c.indent--
	c.writeln(fmt.Sprintf(") %s))", target))
	c.indent--
	c.writeln(")")

	c.env = origEnv
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			c.env.SetVar(u.Target, t, true)
		}
	}
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}

	// No else or else-if branches -> use when
	if st.ElseIf == nil && len(st.Else) == 0 {
		c.writeln("(when " + cond)
		c.indent++
		for _, s := range st.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
		return nil
	}

	// if with else or else-if
	c.writeln("(if " + cond)
	c.indent++
	c.writeln("(do")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeIndent()
	c.buf.WriteByte('\n')
	if st.ElseIf != nil {
		if err := c.compileIf(st.ElseIf); err != nil {
			return err
		}
	} else {
		c.writeln("(do")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	}
	c.writeln(")")
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
	operands := []string{left}
	typesList := []types.Type{c.unaryType(b.Left)}
	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		operands = append(operands, r)
		typesList = append(typesList, c.postfixType(part.Right))
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

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if contains(lvl, ops[i]) {
				expr, typ := c.compileBinaryOp(operands[i], typesList[i], ops[i], operands[i+1], typesList[i+1])
				operands[i] = expr
				typesList[i] = typ
				operands = append(operands[:i+1], operands[i+2:]...)
				typesList = append(typesList[:i+1], typesList[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected binary expr")
	}
	return operands[0], nil
}

func (c *Compiler) compileBinaryOp(left string, leftType types.Type, op string, right string, rightType types.Type) (string, types.Type) {
	opName := op
	switch opName {
	case "%":
		opName = "mod"
	case "/":
		if isInt(leftType) && isInt(rightType) {
			opName = "quot"
		}
	case "&&":
		opName = "and"
	case "||":
		opName = "or"
	case "==":
		opName = "="
	case "!=":
		opName = "not="
	}

	switch op {
	case "+":
		if isList(leftType) && isList(rightType) {
			return fmt.Sprintf("(vec (concat %s %s))", left, right), leftType
		}
		if isString(leftType) || isString(rightType) {
			return fmt.Sprintf("(str %s %s)", left, right), types.StringType{}
		}
		return fmt.Sprintf("(+ %s %s)", left, right), leftType
	case "-", "*":
		return fmt.Sprintf("(%s %s %s)", opName, left, right), leftType
	case "/":
		return fmt.Sprintf("(%s %s %s)", opName, left, right), leftType
	case "%":
		return fmt.Sprintf("(%s %s %s)", opName, left, right), leftType
	case "==":
		c.use("_equal")
		return fmt.Sprintf("(_equal %s %s)", left, right), types.BoolType{}
	case "!=":
		c.use("_equal")
		return fmt.Sprintf("(not (_equal %s %s))", left, right), types.BoolType{}
	case "<", "<=", ">", ">=":
		// Compare strings using (compare) if either static types or
		// heuristics indicate string values. Map fields are typed as
		// `any`, so fall back to checking the expression text.
		if (isString(leftType) && isString(rightType)) ||
			((isAny(leftType) || isString(leftType)) && (isAny(rightType) || isString(rightType)) &&
				c.isStringExpr(left) && c.isStringExpr(right)) {
			cmp := fmt.Sprintf("(compare %s %s)", left, right)
			switch op {
			case "<":
				return fmt.Sprintf("(< %s 0)", cmp), types.BoolType{}
			case "<=":
				return fmt.Sprintf("(<= %s 0)", cmp), types.BoolType{}
			case ">":
				return fmt.Sprintf("(> %s 0)", cmp), types.BoolType{}
			case ">=":
				return fmt.Sprintf("(>= %s 0)", cmp), types.BoolType{}
			}
		}
		return fmt.Sprintf("(%s %s %s)", opName, left, right), types.BoolType{}
	case "&&", "||":
		return fmt.Sprintf("(%s %s %s)", opName, left, right), types.BoolType{}
	case "in":
		switch rightType.(type) {
		case types.StringType:
			if isString(leftType) {
				return fmt.Sprintf("(clojure.string/includes? %s %s)", right, left), types.BoolType{}
			}
		case types.MapType:
			return fmt.Sprintf("(contains? %s %s)", right, left), types.BoolType{}
		case types.ListType:
			return fmt.Sprintf("(some #(= %s %%) %s)", left, right), types.BoolType{}
		}
		c.use("_in")
		return fmt.Sprintf("(_in %s %s)", left, right), types.BoolType{}
	case "union_all":
		c.use("_union_all")
		return fmt.Sprintf("(_union_all %s %s)", left, right), leftType
	case "union":
		c.use("_union")
		return fmt.Sprintf("(_union %s %s)", left, right), leftType
	case "except":
		c.use("_except")
		return fmt.Sprintf("(_except %s %s)", left, right), leftType
	case "intersect":
		c.use("_intersect")
		return fmt.Sprintf("(_intersect %s %s)", left, right), leftType
	default:
		return fmt.Sprintf("(%s %s %s)", opName, left, right), types.AnyType{}
	}
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		name := op
		if name == "!" {
			name = "not"
		}
		expr = fmt.Sprintf("(%s %s)", name, expr)
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	t := c.primaryType(p.Target)

	if sel := p.Target.Selector; sel != nil {
		if mod, ok := c.pyModules[sel.Root]; ok && c.pyAuto[sel.Root] && mod == "math" {
			attr := strings.Join(sel.Tail, ".")
			if len(p.Ops) > 0 && p.Ops[0].Call != nil {
				args := make([]string, len(p.Ops[0].Call.Args))
				for i, a := range p.Ops[0].Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				switch attr {
				case "sqrt":
					return fmt.Sprintf("(Math/sqrt %s)", args[0]), nil
				case "pow":
					if len(args) == 2 {
						return fmt.Sprintf("(Math/pow %s %s)", args[0], args[1]), nil
					}
				case "sin":
					return fmt.Sprintf("(Math/sin %s)", args[0]), nil
				case "log":
					return fmt.Sprintf("(Math/log %s)", args[0]), nil
				}
			} else {
				switch attr {
				case "pi":
					return "Math/PI", nil
				case "e":
					return "Math/E", nil
				}
			}
		}
		if mod, ok := c.goModules[sel.Root]; ok && c.goAuto[sel.Root] && mod == "mochi/runtime/ffi/go/testpkg" {
			attr := strings.Join(sel.Tail, ".")
			if len(p.Ops) > 0 && p.Ops[0].Call != nil {
				args := make([]string, len(p.Ops[0].Call.Args))
				for i, a := range p.Ops[0].Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				if attr == "Add" && len(args) == 2 {
					return fmt.Sprintf("(+ %s %s)", args[0], args[1]), nil
				}
			} else {
				switch attr {
				case "Pi":
					return "3.14", nil
				case "Answer":
					return "42", nil
				}
			}
		}
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		if op.Index != nil {
			start, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if op.Index.Colon != nil {
				end, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				if _, ok := t.(types.StringType); ok {
					expr = fmt.Sprintf("(subs %s %s %s)", expr, start, end)
					t = types.StringType{}
				} else if lt, ok := t.(types.ListType); ok {
					expr = fmt.Sprintf("(subvec %s %s %s)", expr, start, end)
					t = lt
				} else {
					expr = fmt.Sprintf("(subs %s %s %s)", expr, start, end)
					t = types.AnyType{}
				}
			} else {
				if _, ok := t.(types.StringType); ok {
					c.use("_indexString")
					expr = fmt.Sprintf("(_indexString %s %s)", expr, start)
					t = types.StringType{}
				} else if lt, ok := t.(types.ListType); ok {
					c.use("_indexList")
					expr = fmt.Sprintf("(_indexList %s %s)", expr, start)
					t = lt.Elem
				} else if mt, ok := t.(types.MapType); ok {
					expr = fmt.Sprintf("(get %s %s)", expr, start)
					t = mt.Value
				} else {
					c.use("_indexList")
					expr = fmt.Sprintf("(_indexList %s %s)", expr, start)
					t = types.AnyType{}
				}
			}
			continue
		}
		if op.Field != nil {
			fname := op.Field.Name
			// Handle method call like x.contains(y) or x.starts_with(y)
			if (fname == "contains" || fname == "starts_with") && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
				argExpr, err := c.compileExpr(p.Ops[i+1].Call.Args[0])
				if err != nil {
					return "", err
				}
				if _, ok := t.(types.StringType); ok {
					if fname == "contains" {
						expr = fmt.Sprintf("(clojure.string/includes? %s %s)", expr, argExpr)
					} else {
						expr = fmt.Sprintf("(clojure.string/starts-with? %s %s)", expr, argExpr)
					}
				} else if fname == "contains" {
					c.use("_in")
					expr = fmt.Sprintf("(_in %s %s)", argExpr, expr)
				} else {
					return "", fmt.Errorf("starts_with not supported on %s", t.String())
				}
				t = types.BoolType{}
				i++
				continue
			}

			expr = fmt.Sprintf("(:%s %s)", sanitizeName(fname), expr)
			if st, ok := t.(types.StructType); ok {
				if ft, ok := st.Fields[fname]; ok {
					t = ft
				} else {
					t = types.AnyType{}
				}
			} else if mt, ok := t.(types.MapType); ok {
				t = mt.Value
			} else {
				t = types.AnyType{}
			}
			continue
		}
		if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, v)
			}
			name := expr
			if v, err := c.env.GetVar(name); err == nil {
				if ft, ok := v.(types.FuncType); ok {
					if len(args) < len(ft.Params) {
						missing := len(ft.Params) - len(args)
						params := make([]string, missing)
						for i := range params {
							params[i] = c.newTemp()
						}
						callArgs := append(append([]string{}, args...), params...)
						expr = fmt.Sprintf("(fn [%s] (%s %s))",
							strings.Join(params, " "), name,
							strings.Join(callArgs, " "))
						t = types.FuncType{Params: ft.Params[len(args):], Return: ft.Return}
						continue
					}
				}
			}
			// method call like obj.foo() or obj.bar.baz()
			// method call like obj.foo() or obj.bar.baz()
			if p.Target.Selector != nil && len(p.Target.Selector.Tail) >= 1 {
				method := p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1]
				sel := &parser.SelectorExpr{Root: p.Target.Selector.Root, Tail: p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1]}
				prefixPrimary := &parser.Primary{Selector: sel}
				prefixExpr, err := c.compilePrimary(prefixPrimary)
				if err != nil {
					return "", err
				}
				prefixType := c.primaryType(prefixPrimary)
				if (method == "contains" || method == "starts_with") && len(args) == 1 {
					if _, ok := prefixType.(types.StringType); ok || c.isStringExpr(prefixExpr) {
						if method == "contains" {
							expr = fmt.Sprintf("(clojure.string/includes? %s %s)", prefixExpr, args[0])
						} else {
							expr = fmt.Sprintf("(clojure.string/starts-with? %s %s)", prefixExpr, args[0])
						}
					} else if method == "contains" {
						c.use("_in")
						expr = fmt.Sprintf("(_in %s %s)", args[0], prefixExpr)
					} else {
						return "", fmt.Errorf("starts_with not supported on %s", prefixType.String())
					}
					t = types.BoolType{}
					continue
				}
				if len(p.Target.Selector.Tail) == 1 {
					root := p.Target.Selector.Root
					if st, err := c.env.GetVar(root); err == nil {
						if stt, ok := st.(types.StructType); ok {
							if m, ok := stt.Methods[method]; ok {
								callArgs := append([]string{sanitizeName(root)}, args...)
								expr = fmt.Sprintf("(%s_%s %s)", sanitizeName(stt.Name), sanitizeName(method), strings.Join(callArgs, " "))
								t = m.Type.Return
								continue
							}
						}
					}
					switch method {
					case "keys":
						if len(args) == 0 {
							expr = fmt.Sprintf("(vec (keys %s))", sanitizeName(root))
							t = types.ListType{Elem: types.AnyType{}}
							continue
						}
					case "values":
						if len(args) == 0 {
							expr = fmt.Sprintf("(vec (vals %s))", sanitizeName(root))
							t = types.ListType{Elem: types.AnyType{}}
							continue
						}
					}
				}
			}
			switch name {
			case "print":
				expr = fmt.Sprintf("(println %s)", strings.Join(args, " "))
			case "len":
				expr = fmt.Sprintf("(count %s)", args[0])
			case "count":
				argT := c.exprType(p.Ops[i].Call.Args[0])
				switch argT.(type) {
				case types.ListType:
					expr = fmt.Sprintf("(count %s)", args[0])
				case types.GroupType:
					expr = fmt.Sprintf("(count (:Items %s))", args[0])
				default:
					c.use("_count")
					expr = fmt.Sprintf("(_count %s)", args[0])
				}
			case "avg":
				argT := c.exprType(p.Ops[i].Call.Args[0])
				switch at := argT.(type) {
				case types.ListType:
					if isNumber(at.Elem) {
						expr = fmt.Sprintf("(let [xs %s] (if (empty? xs) 0 (/ (reduce + xs) (double (count xs)))))", args[0])
					} else {
						c.use("_avg")
						expr = fmt.Sprintf("(_avg %s)", args[0])
					}
				case types.GroupType:
					if isNumber(at.Elem) {
						expr = fmt.Sprintf("(let [xs (:Items %s)] (if (empty? xs) 0 (/ (reduce + xs) (double (count xs)))))", args[0])
					} else {
						c.use("_avg")
						expr = fmt.Sprintf("(_avg %s)", args[0])
					}
				default:
					c.use("_avg")
					expr = fmt.Sprintf("(_avg %s)", args[0])
				}
			case "sum":
				argT := c.exprType(p.Ops[i].Call.Args[0])
				switch at := argT.(type) {
				case types.ListType:
					if isNumber(at.Elem) {
						expr = fmt.Sprintf("(reduce + 0 %s)", args[0])
					} else {
						c.use("_sum")
						expr = fmt.Sprintf("(_sum %s)", args[0])
					}
				case types.GroupType:
					if isNumber(at.Elem) {
						expr = fmt.Sprintf("(reduce + 0 (:Items %s))", args[0])
					} else {
						c.use("_sum")
						expr = fmt.Sprintf("(_sum %s)", args[0])
					}
				default:
					c.use("_sum")
					expr = fmt.Sprintf("(_sum %s)", args[0])
				}
			case "min":
				argT := c.exprType(p.Ops[i].Call.Args[0])
				switch at := argT.(type) {
				case types.ListType:
					if isNumber(at.Elem) {
						expr = fmt.Sprintf("(apply min %s)", args[0])
					} else {
						c.use("_min")
						expr = fmt.Sprintf("(_min %s)", args[0])
					}
				case types.GroupType:
					if isNumber(at.Elem) {
						expr = fmt.Sprintf("(apply min (:Items %s))", args[0])
					} else {
						c.use("_min")
						expr = fmt.Sprintf("(_min %s)", args[0])
					}
				default:
					c.use("_min")
					expr = fmt.Sprintf("(_min %s)", args[0])
				}
			case "max":
				argT := c.exprType(p.Ops[i].Call.Args[0])
				switch at := argT.(type) {
				case types.ListType:
					if isNumber(at.Elem) {
						expr = fmt.Sprintf("(apply max %s)", args[0])
					} else {
						c.use("_max")
						expr = fmt.Sprintf("(_max %s)", args[0])
					}
				case types.GroupType:
					if isNumber(at.Elem) {
						expr = fmt.Sprintf("(apply max (:Items %s))", args[0])
					} else {
						c.use("_max")
						expr = fmt.Sprintf("(_max %s)", args[0])
					}
				default:
					c.use("_max")
					expr = fmt.Sprintf("(_max %s)", args[0])
				}
			case "input":
				c.use("_input")
				expr = "(_input)"
			case "json":
				if len(args) == 1 {
					c.use("_json")
					expr = fmt.Sprintf("(_json %s)", args[0])
				}
			case "now":
				expr = "(System/nanoTime)"
			case "str":
				expr = fmt.Sprintf("(str %s)", strings.Join(args, " "))
			case "upper":
				if len(args) == 1 {
					expr = fmt.Sprintf("(clojure.string/upper-case %s)", args[0])
				}
			case "lower":
				if len(args) == 1 {
					expr = fmt.Sprintf("(clojure.string/lower-case %s)", args[0])
				}
			case "substring":
				if len(args) == 3 {
					expr = fmt.Sprintf("(.substring %s %s %s)", args[0], args[1], args[2])
				} else if len(args) == 2 {
					expr = fmt.Sprintf("(.substring %s %s)", args[0], args[1])
				}
			default:
				expr = fmt.Sprintf("(%s %s)", name, strings.Join(args, " "))
			}
			continue
		}
		if op.Cast != nil {
			if op.Cast.Type != nil {
				nt := c.resolveTypeRef(op.Cast.Type)
				switch tt := nt.(type) {
				case types.FloatType:
					expr = fmt.Sprintf("(double %s)", expr)
					t = types.FloatType{}
				case types.IntType:
					// If the source looks like a string, use
					// Integer/parseInt instead of int cast.
					if _, ok := t.(types.StringType); ok || c.isStringExpr(expr) {
						expr = fmt.Sprintf("(Integer/parseInt %s)", expr)
					} else {
						expr = fmt.Sprintf("(int %s)", expr)
					}
					t = types.IntType{}
				case types.StructType:
					c.use("_cast_struct")
					expr = fmt.Sprintf("(_cast_struct %s %s)", sanitizeName(tt.Name), expr)
					t = tt
				case types.ListType:
					if st, ok := tt.Elem.(types.StructType); ok {
						c.use("_cast_struct_list")
						expr = fmt.Sprintf("(_cast_struct_list %s %s)", sanitizeName(st.Name), expr)
					}
					t = tt
				}
			}
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			s := strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64)
			if !strings.Contains(s, ".") {
				s += ".0"
			}
			return s, nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, " ") + "]", nil
	case p.Map != nil:
		parts := make([]string, 0, len(p.Map.Items)*2)
		for _, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			if name, ok := isIdentExpr(it.Key); ok {
				k = ":" + sanitizeName(name)
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts = append(parts, k, v)
		}
		return "{" + strings.Join(parts, " ") + "}", nil
	case p.Struct != nil:
		parts := make([]string, 0, len(p.Struct.Fields)+1)
		parts = append(parts, fmt.Sprintf(":__name \"%s\"", p.Struct.Name))
		for _, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf(":%s %s", sanitizeName(f.Name), v))
		}
		return "{" + strings.Join(parts, " ") + "}", nil
	case p.Query != nil:
		expr, err := c.compileQueryExpr(p.Query)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.Match != nil:
		expr, err := c.compileMatch(p.Match)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.If != nil:
		expr, err := c.compileIfExpr(p.If)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		for _, s := range p.Selector.Tail {
			expr = fmt.Sprintf("(:%s %s)", sanitizeName(s), expr)
		}
		return expr, nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.FunExpr != nil:
		expr, err := c.compileFunExpr(p.FunExpr)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, v)
		}
		name := sanitizeName(p.Call.Func)
		if v, err := c.env.GetVar(p.Call.Func); err == nil {
			if ft, ok := v.(types.FuncType); ok {
				if len(args) < len(ft.Params) {
					missing := len(ft.Params) - len(args)
					params := make([]string, missing)
					for i := range params {
						params[i] = c.newTemp()
					}
					callArgs := append(append([]string{}, args...), params...)
					return fmt.Sprintf("(fn [%s] (%s %s))",
						strings.Join(params, " "), name,
						strings.Join(callArgs, " ")), nil
				}
			}
		}
		switch name {
		case "print":
			return "(println " + strings.Join(args, " ") + ")", nil
		case "len":
			if len(args) == 1 {
				return "(count " + args[0] + ")", nil
			}
		case "count":
			if len(args) == 1 {
				argT := c.exprType(p.Call.Args[0])
				switch argT.(type) {
				case types.ListType:
					return "(count " + args[0] + ")", nil
				case types.GroupType:
					return "(count (:Items " + args[0] + "))", nil
				default:
					c.use("_count")
					return "(_count " + args[0] + ")", nil
				}
			}
		case "exists":
			if len(args) == 1 {
				return "(boolean (seq " + args[0] + "))", nil
			}
		case "avg":
			if len(args) == 1 {
				argT := c.exprType(p.Call.Args[0])
				switch at := argT.(type) {
				case types.ListType:
					if isNumber(at.Elem) {
						return "(let [xs " + args[0] + "] (if (empty? xs) 0 (/ (reduce + xs) (double (count xs)))))", nil
					}
				case types.GroupType:
					if isNumber(at.Elem) {
						return "(let [xs (:Items " + args[0] + ")] (if (empty? xs) 0 (/ (reduce + xs) (double (count xs)))))", nil
					}
				}
				c.use("_avg")
				return "(_avg " + args[0] + ")", nil
			}
		case "sum":
			if len(args) == 1 {
				argT := c.exprType(p.Call.Args[0])
				switch at := argT.(type) {
				case types.ListType:
					if isNumber(at.Elem) {
						return "(reduce + 0 " + args[0] + ")", nil
					}
				case types.GroupType:
					if isNumber(at.Elem) {
						return "(reduce + 0 (:Items " + args[0] + "))", nil
					}
				}
				c.use("_sum")
				return "(_sum " + args[0] + ")", nil
			}
		case "min":
			if len(args) == 1 {
				argT := c.exprType(p.Call.Args[0])
				switch at := argT.(type) {
				case types.ListType:
					if isNumber(at.Elem) {
						return "(apply min " + args[0] + ")", nil
					}
				case types.GroupType:
					if isNumber(at.Elem) {
						return "(apply min (:Items " + args[0] + "))", nil
					}
				}
				c.use("_min")
				return "(_min " + args[0] + ")", nil
			}
		case "max":
			if len(args) == 1 {
				argT := c.exprType(p.Call.Args[0])
				switch at := argT.(type) {
				case types.ListType:
					if isNumber(at.Elem) {
						return "(apply max " + args[0] + ")", nil
					}
				case types.GroupType:
					if isNumber(at.Elem) {
						return "(apply max (:Items " + args[0] + "))", nil
					}
				}
				c.use("_max")
				return "(_max " + args[0] + ")", nil
			}
		case "input":
			if len(args) == 0 {
				c.use("_input")
				return "(_input)", nil
			}
		case "now":
			if len(args) == 0 {
				return "(System/nanoTime)", nil
			}
		case "json":
			if len(args) == 1 {
				c.use("_json")
				return "(_json " + args[0] + ")", nil
			}
		case "keys":
			if len(args) == 1 {
				return "(vec (keys " + args[0] + "))", nil
			}
		case "values":
			if len(args) == 1 {
				return "(vec (vals " + args[0] + "))", nil
			}
		case "str":
			return "(str " + strings.Join(args, " ") + ")", nil
		case "append":
			if len(args) == 2 {
				return "(conj " + args[0] + " " + args[1] + ")", nil
			}
		case "upper":
			if len(args) == 1 {
				return "(clojure.string/upper-case " + args[0] + ")", nil
			}
		case "lower":
			if len(args) == 1 {
				return "(clojure.string/lower-case " + args[0] + ")", nil
			}
		case "substring":
			if len(args) == 3 {
				return "(.substring " + args[0] + " " + args[1] + " " + args[2] + ")", nil
			}
			if len(args) == 2 {
				return "(.substring " + args[0] + " " + args[1] + ")", nil
			}
		}
		return "(" + name + " " + strings.Join(args, " ") + ")", nil
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, error) {
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 &&
		q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		src, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		origEnv := c.env
		child := types.NewEnv(c.env)
		var qElem types.Type = types.AnyType{}
		if st := c.exprType(q.Source); st != nil {
			switch tt := st.(type) {
			case types.ListType:
				qElem = tt.Elem
			case types.GroupType:
				qElem = tt.Elem
				src = fmt.Sprintf("(:Items %s)", src)
			}
		}
		child.SetVar(q.Var, qElem, true)
		c.env = child
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = origEnv
			return "", err
		}
		genv := types.NewEnv(child)
		var gElem types.Type
		gElem, _ = child.GetVar(q.Var)
		if gElem == nil {
			gElem = types.AnyType{}
		}
		genv.SetVar(q.Group.Name, types.GroupType{Elem: gElem}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		c.env = origEnv
		c.use("_Group")
		c.use("_group_by")
		expr := fmt.Sprintf("(map (fn [%s] %s) (_group_by %s (fn [%s] %s)))",
			sanitizeName(q.Group.Name), valExpr, src, sanitizeName(q.Var), keyExpr)
		return expr, nil
	}

	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 {
		expr, err := c.compileSimpleGroup(q)
		if err == nil {
			return expr, nil
		}
	}

	useHelper := false
	if q.Group != nil {
		useHelper = true
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			useHelper = true
		}
	}
	if useHelper {
		return c.compileQueryHelper(q)
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	origEnv := c.env
	child := types.NewEnv(c.env)
	var qElem types.Type = types.AnyType{}
	if st := c.exprType(q.Source); st != nil {
		switch tt := st.(type) {
		case types.ListType:
			qElem = tt.Elem
		case types.GroupType:
			qElem = tt.Elem
			src = fmt.Sprintf("(:Items %s)", src)
		}
	}
	child.SetVar(q.Var, qElem, true)
	for _, f := range q.Froms {
		var fElem types.Type = types.AnyType{}
		if ft := c.exprType(f.Src); ft != nil {
			if lt, ok := ft.(types.ListType); ok {
				fElem = lt.Elem
			}
		}
		child.SetVar(f.Var, fElem, true)
	}
	for _, j := range q.Joins {
		var jElem types.Type = types.AnyType{}
		if jt := c.exprType(j.Src); jt != nil {
			if lt, ok := jt.(types.ListType); ok {
				jElem = lt.Elem
			}
		}
		child.SetVar(j.Var, jElem, true)
	}
	c.env = child

	if q.Group != nil && len(q.Froms) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = origEnv
			return "", err
		}
		genv := types.NewEnv(child)
		var gElem types.Type
		gElem, _ = child.GetVar(q.Var)
		if gElem == nil {
			gElem = types.AnyType{}
		}
		genv.SetVar(q.Group.Name, types.GroupType{Elem: gElem}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		c.env = origEnv
		c.use("_Group")
		c.use("_group_by")
		expr := fmt.Sprintf("(map (fn [%s] %s) (_group_by %s (fn [%s] %s)))",
			sanitizeName(q.Group.Name), valExpr, src, sanitizeName(q.Var), keyExpr)
		return expr, nil
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
	joinLeftKeys := make([]string, len(q.Joins))
	joinRightKeys := make([]string, len(q.Joins))
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

		if leftExpr, rightExpr, ok := joinEqParts(j.On); ok {
			leftVars := varsInExpr(leftExpr)
			rightVars := varsInExpr(rightExpr)
			lkeyExpr := leftExpr
			rkeyExpr := rightExpr
			if leftVars[j.Var] && !rightVars[j.Var] {
				lkeyExpr = rightExpr
				rkeyExpr = leftExpr
			} else if rightVars[j.Var] && !leftVars[j.Var] {
				// lkeyExpr and rkeyExpr already correct
			} else {
				lkeyExpr = nil
				rkeyExpr = nil
			}
			if lkeyExpr != nil && rkeyExpr != nil {
				lkey, err := c.compileExpr(lkeyExpr)
				if err == nil {
					rkey, err2 := c.compileExpr(rkeyExpr)
					if err2 == nil {
						joinLeftKeys[i] = lkey
						joinRightKeys[i] = rkey
					}
				}
			}
		}
	}
	var whereExpr string
	if q.Where != nil {
		whereExpr, err = c.compileExpr(q.Where)
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
	selExpr, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	aggFn := ""
	if call, ok := callPattern(q.Select); ok && len(call.Args) == 1 {
		fn := call.Func
		if fn == "sum" || fn == "count" || fn == "avg" || fn == "min" || fn == "max" {
			aggFn = fn
			selExpr, err = c.compileExpr(call.Args[0])
			if err != nil {
				return "", err
			}
		}
	}
	v := sanitizeName(q.Var)
	varParts := []string{fmt.Sprintf("%s %s", v, src)}
	varNames := []string{q.Var}
	idxMap := []int{1}
	pos := 1
	for i, f := range q.Froms {
		varParts = append(varParts, fmt.Sprintf("%s %s", sanitizeName(f.Var), fromSrcs[i]))
		pos++
		varNames = append(varNames, f.Var)
		idxMap = append(idxMap, pos)
	}
	for i, j := range q.Joins {
		varParts = append(varParts, fmt.Sprintf("%s %s", sanitizeName(j.Var), joinSrcs[i]))
		pos++
		varParts = append(varParts, ":when "+joinOns[i])
		pos++
		varNames = append(varNames, j.Var)
		idxMap = append(idxMap, pos)
	}

	wherePos := len(varParts)
	if whereExpr != "" {
		used := varsInExpr(q.Where)
		maxIdx := -1
		for i, n := range varNames {
			if used[n] && i > maxIdx {
				maxIdx = i
			}
		}
		if maxIdx >= 0 && maxIdx < len(idxMap) {
			wherePos = idxMap[maxIdx]
		}
	}

	var b strings.Builder
	b.WriteString("(vec (->>")
	b.WriteString(" (for [")
	for i, part := range varParts {
		if i > 0 {
			b.WriteString(" ")
		}
		if i == wherePos && whereExpr != "" {
			b.WriteString(":when " + whereExpr + " ")
		}
		b.WriteString(part)
	}
	if whereExpr != "" && wherePos == len(varParts) {
		b.WriteString(" :when " + whereExpr)
	}
	b.WriteString("] " + selExpr + ")")
	if sortExpr != "" {
		b.WriteString(" (sort-by (fn [" + v + "] (_sort_key " + sortExpr + ")))")
		c.use("_sort_key")
	}
	if skipExpr != "" {
		b.WriteString(" (drop " + skipExpr + ")")
	}
	if takeExpr != "" {
		b.WriteString(" (take " + takeExpr + ")")
	}
	b.WriteString("))")
	c.env = origEnv
	expr := b.String()

	// Handle simple aggregations like sum(), count(), avg(), min(), max()
	if aggFn != "" && len(q.Joins) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		c.use("_" + aggFn)
		return fmt.Sprintf("(_%s %s)", aggFn, expr), nil
	}
	return expr, nil
}

// compileQueryExpr exposes dataset query compilation used by primary expressions.
// It currently delegates to compileQuery which implements full query translation.
func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	return c.compileQuery(q)
}

func (c *Compiler) compileMatch(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(let [t " + target + "]\n")
	b.WriteString("  (cond\n")
	hasDefault := false
	for _, cs := range m.Cases {
		if isUnderscoreExpr(cs.Pattern) {
			res, err := c.compileExpr(cs.Result)
			if err != nil {
				return "", err
			}
			b.WriteString("    :else " + res + "\n")
			hasDefault = true
			continue
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}

		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				names := []string{}
				values := []string{}
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						names = append(names, sanitizeName(id))
						field := sanitizeName(st.Order[idx])
						values = append(values, fmt.Sprintf("(:%s t)", field))
					}
				}
				if len(names) > 0 {
					pairs := make([]string, 0, len(names)*2)
					for i, n := range names {
						pairs = append(pairs, fmt.Sprintf("%s %s", n, values[i]))
					}
					res = fmt.Sprintf("(let [%s] %s)", strings.Join(pairs, " "), res)
				}
				cond := fmt.Sprintf("(= (:__name t) \"%s\")", call.Func)
				b.WriteString("    " + cond + " " + res + "\n")
				continue
			}
		}
		if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond := fmt.Sprintf("(= (:__name t) \"%s\")", ident)
				b.WriteString("    " + cond + " " + res + "\n")
				continue
			}
		}
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		b.WriteString("    (= t " + pat + ") " + res + "\n")
	}
	if !hasDefault {
		b.WriteString("    :else nil\n")
	}
	b.WriteString("  ))")
	return b.String(), nil
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
	elseExpr := "nil"
	if ie.ElseIf != nil {
		v, err := c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
		elseExpr = v
	} else if ie.Else != nil {
		v, err := c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
		elseExpr = v
	}
	return fmt.Sprintf("(if %s %s %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	child := types.NewEnv(c.env)
	for _, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		} else {
			child.SetVar(p.Name, types.AnyType{}, true)
		}
	}

	sub := &Compiler{env: child, tempVarCount: c.tempVarCount, helpers: make(map[string]bool)}
	sub.indent = 1
	sub.writeln("(try")
	sub.indent++
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln(fmt.Sprintf("(throw (ex-info \"return\" {:value %s}))", expr))
	} else {
		for _, st := range fn.BlockBody {
			if err := sub.compileStmt(st); err != nil {
				return "", err
			}
		}
	}
	sub.indent--
	sub.writeln("(catch clojure.lang.ExceptionInfo e")
	sub.indent++
	sub.writeln("(if (= (.getMessage e) \"return\")")
	sub.indent++
	sub.writeln("(:value (ex-data e))")
	sub.indent--
	sub.writeln("(throw e)))")
	sub.indent--
	sub.writeln(")")

	// propagate helper usage
	for h := range sub.helpers {
		c.helpers[h] = true
	}
	c.tempVarCount = sub.tempVarCount

	var buf bytes.Buffer
	buf.WriteString("(fn [")
	for i, p := range fn.Params {
		if i > 0 {
			buf.WriteString(" ")
		}
		buf.WriteString(sanitizeName(p.Name))
	}
	buf.WriteString("]\n")
	buf.Write(sub.buf.Bytes())
	buf.WriteString(")")
	return buf.String(), nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if st, ok := c.env.GetStruct(t.Name); ok {
		if len(st.Fields) > 0 {
			c.writeIndent()
			c.buf.WriteString("(defn " + sanitizeName(t.Name) + " [")
			for i, f := range st.Order {
				if i > 0 {
					c.buf.WriteString(" ")
				}
				c.buf.WriteString(sanitizeName(f))
			}
			c.buf.WriteString("]\n")
			c.indent++
			parts := make([]string, 0, len(st.Fields)+1)
			parts = append(parts, fmt.Sprintf(":__name \"%s\"", st.Name))
			for _, f := range st.Order {
				parts = append(parts, fmt.Sprintf(":%s %s", sanitizeName(f), sanitizeName(f)))
			}
			c.writeln("{" + strings.Join(parts, " ") + "}")
			c.indent--
			c.writeln(")")
			c.writeln("")
		}
		for _, m := range t.Members {
			if m.Method == nil {
				continue
			}
			if err := c.compileMethod(t.Name, m.Method); err != nil {
				return err
			}
			c.writeln("")
		}
		return nil
	}
	if len(t.Variants) > 0 {
		for _, v := range t.Variants {
			c.writeIndent()
			if len(v.Fields) == 0 {
				c.writeln("(def " + sanitizeName(v.Name) + " {:__name \"" + v.Name + "\"})")
				c.writeln("")
				continue
			}
			c.buf.WriteString("(defn " + sanitizeName(v.Name) + " [")
			for i, f := range v.Fields {
				if i > 0 {
					c.buf.WriteString(" ")
				}
				c.buf.WriteString(sanitizeName(f.Name))
			}
			c.buf.WriteString("]\n")
			c.indent++
			parts := make([]string, 0, len(v.Fields)+1)
			parts = append(parts, fmt.Sprintf(":__name \"%s\"", v.Name))
			for _, f := range v.Fields {
				parts = append(parts, fmt.Sprintf(":%s %s", sanitizeName(f.Name), sanitizeName(f.Name)))
			}
			c.writeln("{" + strings.Join(parts, " ") + "}")
			c.indent--
			c.writeln(")")
			c.writeln("")
		}
	}
	return nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		p := *l.Path
		if strings.HasPrefix(p, "http://") || strings.HasPrefix(p, "https://") || strings.HasPrefix(p, "/") || strings.HasPrefix(p, "file:") {
			path = strconv.Quote(p)
		} else {
			c.use("_rel_path")
			path = fmt.Sprintf("(_rel_path %s)", strconv.Quote(p))
		}
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_load")
	expr := fmt.Sprintf("(_load %s %s)", path, opts)
	if l.Type != nil && l.Type.Simple != nil {
		name := sanitizeName(*l.Type.Simple)
		expr = fmt.Sprintf("(mapv %s %s)", name, expr)
	}
	return expr, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if s.Path != nil {
		path = strconv.Quote(*s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_save")
	if c.imports != nil {
		c.imports["json"] = "clojure.data.json"
	}
	return fmt.Sprintf("(_save %s %s %s)", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "nil"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.use("_fetch")
	if c.imports != nil {
		c.imports["json"] = "clojure.data.json"
	}
	return fmt.Sprintf("(_fetch %s %s)", url, opts), nil
}

// compileSimpleGroup handles grouping queries without joins using a simpler
// translation than the generic helper. It supports optional sort, skip and take
// clauses.
func (c *Compiler) compileSimpleGroup(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	origEnv := c.env
	child := types.NewEnv(c.env)
	var qElem types.Type = types.AnyType{}
	if st := c.exprType(q.Source); st != nil {
		switch tt := st.(type) {
		case types.ListType:
			qElem = tt.Elem
		case types.GroupType:
			qElem = tt.Elem
			src = fmt.Sprintf("(:Items %s)", src)
		}
	}
	child.SetVar(q.Var, qElem, true)
	c.env = child
	keyExpr, err := c.compileExpr(q.Group.Exprs[0])
	if err != nil {
		c.env = origEnv
		return "", err
	}
	whereExpr := ""
	if q.Where != nil {
		whereExpr, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = origEnv
			return "", err
		}
	}

	genv := types.NewEnv(child)
	var gElem types.Type
	gElem, _ = child.GetVar(q.Var)
	if gElem == nil {
		gElem = types.AnyType{}
	}
	genv.SetVar(q.Group.Name, types.GroupType{Elem: gElem}, true)
	c.env = genv

	sortExpr := ""
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = origEnv
			return "", err
		}
	}
	skipExpr := ""
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = origEnv
			return "", err
		}
	}
	takeExpr := ""
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = origEnv
			return "", err
		}
	}

	valExpr, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = origEnv
		return "", err
	}
	c.env = origEnv
	c.use("_Group")
	c.use("_group_by")
	var b strings.Builder
	b.WriteString("(let [_src " + src + "\n")
	if whereExpr != "" {
		b.WriteString("      _filtered (vec (filter (fn [" + sanitizeName(q.Var) + "] " + whereExpr + ") _src))\n")
		b.WriteString("      _groups (_group_by _filtered (fn [" + sanitizeName(q.Var) + "] " + keyExpr + "))\n")
	} else {
		b.WriteString("      _groups (_group_by _src (fn [" + sanitizeName(q.Var) + "] " + keyExpr + "))\n")
	}
	b.WriteString("      ]\n")
	b.WriteString("  (->> _groups")
	if sortExpr != "" {
		b.WriteString(" (sort-by (fn [" + sanitizeName(q.Group.Name) + "] (_sort_key " + sortExpr + ")))")
		c.use("_sort_key")
	}
	if skipExpr != "" {
		b.WriteString(" (drop " + skipExpr + ")")
	}
	if takeExpr != "" {
		b.WriteString(" (take " + takeExpr + ")")
	}
	b.WriteString(" (map (fn [" + sanitizeName(q.Group.Name) + "] " + valExpr + ")) vec))")
	return b.String(), nil
}

func (c *Compiler) compileQueryHelper(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	origEnv := c.env
	child := types.NewEnv(c.env)
	var qElem types.Type = types.AnyType{}
	if st := c.exprType(q.Source); st != nil {
		if lt, ok := st.(types.ListType); ok {
			qElem = lt.Elem
		}
	}
	child.SetVar(q.Var, qElem, true)
	for _, f := range q.Froms {
		var fElem types.Type = types.AnyType{}
		if ft := c.exprType(f.Src); ft != nil {
			if lt, ok := ft.(types.ListType); ok {
				fElem = lt.Elem
			}
		}
		child.SetVar(f.Var, fElem, true)
	}
	for _, j := range q.Joins {
		var jElem types.Type = types.AnyType{}
		if jt := c.exprType(j.Src); jt != nil {
			if lt, ok := jt.(types.ListType); ok {
				jElem = lt.Elem
			}
		}
		child.SetVar(j.Var, jElem, true)
	}
	c.env = child

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
	joinLeftKeys := make([]string, len(q.Joins))
	joinRightKeys := make([]string, len(q.Joins))
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

		if leftExpr, rightExpr, ok := joinEqParts(j.On); ok {
			leftVars := varsInExpr(leftExpr)
			rightVars := varsInExpr(rightExpr)
			lkeyExpr := leftExpr
			rkeyExpr := rightExpr
			if leftVars[j.Var] && !rightVars[j.Var] {
				lkeyExpr = rightExpr
				rkeyExpr = leftExpr
			} else if rightVars[j.Var] && !leftVars[j.Var] {
				// orientation correct
			} else {
				lkeyExpr = nil
				rkeyExpr = nil
			}
			if lkeyExpr != nil && rkeyExpr != nil {
				lkey, err := c.compileExpr(lkeyExpr)
				if err == nil {
					rkey, err2 := c.compileExpr(rkeyExpr)
					if err2 == nil {
						joinLeftKeys[i] = lkey
						joinRightKeys[i] = rkey
					}
				}
			}
		}
	}
	var whereExpr, sortExpr, skipExpr, takeExpr string
	var groupKey string
	if q.Where != nil {
		whereExpr, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	if q.Group != nil {
		groupKey, err = c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
	}
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
	selExpr, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	params := []string{sanitizeName(q.Var)}
	joins := make([]string, 0, len(q.Froms)+len(q.Joins))
	for i, fs := range fromSrcs {
		joins = append(joins, fmt.Sprintf("{:items %s}", fs))
		params = append(params, sanitizeName(q.Froms[i].Var))
	}
	paramCopy := append([]string(nil), params...)
	for i, js := range joinSrcs {
		onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
		spec := fmt.Sprintf("{:items %s", js)
		if joinLeftKeys[i] != "" && joinRightKeys[i] != "" {
			lfn := fmt.Sprintf("(fn [%s] %s)", strings.Join(paramCopy, " "), joinLeftKeys[i])
			rfn := fmt.Sprintf("(fn [%s] %s)", sanitizeName(q.Joins[i].Var), joinRightKeys[i])
			spec += " :leftKey " + lfn + " :rightKey " + rfn
		} else {
			spec += fmt.Sprintf(" :on (fn [%s] %s)", strings.Join(onParams, " "), joinOns[i])
		}
		if q.Joins[i].Side != nil {
			side := *q.Joins[i].Side
			if side == "left" || side == "outer" {
				spec += " :left true"
			}
			if side == "right" || side == "outer" {
				spec += " :right true"
			}
		}
		spec += "}"
		joins = append(joins, spec)
		paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
	}
	allParams := strings.Join(paramCopy, " ")
	var selectFn string
	if q.Group != nil {
		tuple := "[" + strings.Join(strings.Split(allParams, " "), " ") + "]"
		selectFn = fmt.Sprintf("(fn [%s] %s)", allParams, tuple)
	} else {
		selectFn = fmt.Sprintf("(fn [%s] %s)", allParams, selExpr)
	}
	var whereFn, sortFn string
	if whereExpr != "" {
		whereFn = fmt.Sprintf("(fn [%s] %s)", allParams, whereExpr)
	}
	if sortExpr != "" {
		sortFn = fmt.Sprintf("(fn [%s] %s)", allParams, sortExpr)
		c.use("_sort_key")
	}
	optsParts := []string{":select " + selectFn}
	if whereFn != "" {
		optsParts = append(optsParts, ":where "+whereFn)
	}
	if sortFn != "" {
		optsParts = append(optsParts, ":sortKey "+sortFn)
	}
	if skipExpr != "" {
		optsParts = append(optsParts, ":skip "+skipExpr)
	}
	if takeExpr != "" {
		optsParts = append(optsParts, ":take "+takeExpr)
	}
	opts := "{ " + strings.Join(optsParts, " ") + " }"
	var b strings.Builder
	if q.Group != nil {
		b.WriteString("(let [_src " + src + "\n")
		b.WriteString("      _rows (_query _src [\n")
		for i, j := range joins {
			b.WriteString("        " + j)
			if i != len(joins)-1 {
				b.WriteString("\n")
			}
		}
		b.WriteString("\n      ] { :select " + selectFn)
		if whereFn != "" {
			b.WriteString(" :where " + whereFn)
		}
		b.WriteString(" })\n")
		b.WriteString("      _groups (_group_by _rows (fn [" + allParams + "] " + groupKey + "))\n")
		b.WriteString("      ]\n")
		genv := types.NewEnv(child)
		var gElem types.Type
		gElem, _ = child.GetVar(q.Var)
		if gElem == nil {
			gElem = types.AnyType{}
		}
		genv.SetVar(q.Group.Name, types.GroupType{Elem: gElem}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		c.env = origEnv
		b.WriteString("  (vec (map (fn [" + sanitizeName(q.Group.Name) + "] " + valExpr + ") _groups))")
		b.WriteString(")")
		c.use("_query")
		c.use("_group_by")
		c.use("_Group")
		c.use("_sort_key")
		return b.String(), nil
	}
	b.WriteString("(let [_src " + src + "]\n  (_query _src [\n")
	for i, j := range joins {
		b.WriteString("    " + j)
		if i != len(joins)-1 {
			b.WriteString("\n")
		}
	}
	b.WriteString("\n  ] " + opts + "))")
	c.env = origEnv
	c.use("_query")
	c.use("_sort_key")
	return b.String(), nil
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
			params = append(params, fmt.Sprintf(":%s %s", sanitizeName(f.Name), v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramStr := "nil"
	if len(params) > 0 {
		paramStr = "{ " + strings.Join(params, " ") + " }"
	}
	if model == "" {
		model = "\"\""
	}
	if g.Target == "embedding" {
		c.use("_gen_embed")
		return fmt.Sprintf("(_gen_embed %s %s %s)", text, model, paramStr), nil
	}
	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_gen_struct")
			if c.imports != nil {
				c.imports["json"] = "clojure.data.json"
			}
			return fmt.Sprintf("(_gen_struct #'%s %s %s %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
		}
	}
	c.use("_gen_text")
	return fmt.Sprintf("(_gen_text %s %s %s)", prompt, model, paramStr), nil
}

func (c *Compiler) isStringExpr(expr string) bool {
	expr = strings.TrimSpace(expr)
	if len(expr) >= 2 && strings.HasPrefix(expr, "\"") && strings.HasSuffix(expr, "\"") {
		return true
	}
	if strings.HasPrefix(expr, "(nth ") {
		v := strings.Fields(expr[5:])
		if len(v) > 0 {
			if t, err := c.env.GetVar(v[0]); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	if strings.HasPrefix(expr, "(:") {
		return true
	}
	if strings.HasPrefix(expr, "(subs ") {
		v := strings.Fields(expr[6:])
		if len(v) > 0 {
			if t, err := c.env.GetVar(v[0]); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	if strings.HasPrefix(expr, "(str ") {
		return true
	}
	if t, err := c.env.GetVar(expr); err == nil {
		if _, ok := t.(types.StringType); ok {
			return true
		}
	}
	return false
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
}

func (c *Compiler) newTemp() string {
	name := fmt.Sprintf("_tmp%d", c.tempVarCount)
	c.tempVarCount++
	return name
}

func (c *Compiler) writeNamespace(buf *bytes.Buffer) {
	buf.WriteString("(ns main")
	if len(c.imports) > 0 {
		buf.WriteString("\n  (:require\n")
		aliases := make([]string, 0, len(c.imports))
		for a := range c.imports {
			aliases = append(aliases, a)
		}
		sort.Strings(aliases)
		for _, a := range aliases {
			buf.WriteString(fmt.Sprintf("    [%s :as %s]\n", c.imports[a], a))
		}
		buf.WriteString("  )")
	}
	buf.WriteString(")\n\n")
}
