//go:build archived

package scalacode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Scala source code (limited subset).
type Compiler struct {
	buf       bytes.Buffer
	indent    int
	env       *types.Env
	mainStmts []*parser.Statement
	retType   types.Type

	tempVarCount int
	loopStack    []loopLabels

	helpers map[string]bool

	paramAlias map[string]string

	pyModules map[string]string
}

type loopLabels struct {
	brk  string
	cont string
}

func (c *Compiler) newTemp(prefix string) string {
	c.tempVarCount++
	return fmt.Sprintf("%s%d", prefix, c.tempVarCount)
}

func containsBreak(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if s.Break != nil {
			return true
		}
		switch {
		case s.If != nil:
			if containsBreak(s.If.Then) || containsBreak(s.If.Else) || (s.If.ElseIf != nil && containsBreak([]*parser.Statement{{If: s.If.ElseIf}})) {
				return true
			}
		case s.For != nil:
			if containsBreak(s.For.Body) {
				return true
			}
		case s.While != nil:
			if containsBreak(s.While.Body) {
				return true
			}
		case s.Fun != nil:
			if containsBreak(s.Fun.Body) {
				return true
			}
		case s.Test != nil:
			if containsBreak(s.Test.Body) {
				return true
			}
		}
	}
	return false
}

func containsContinue(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if s.Continue != nil {
			return true
		}
		switch {
		case s.If != nil:
			if containsContinue(s.If.Then) || containsContinue(s.If.Else) || (s.If.ElseIf != nil && containsContinue([]*parser.Statement{{If: s.If.ElseIf}})) {
				return true
			}
		case s.For != nil:
			if containsContinue(s.For.Body) {
				return true
			}
		case s.While != nil:
			if containsContinue(s.While.Body) {
				return true
			}
		case s.Fun != nil:
			if containsContinue(s.Fun.Body) {
				return true
			}
		case s.Test != nil:
			if containsContinue(s.Test.Body) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) compileBreak() {
	if len(c.loopStack) == 0 {
		return
	}
	lbl := c.loopStack[len(c.loopStack)-1]
	if lbl.brk != "" {
		c.writeln(lbl.brk + ".break()")
	}
}

func (c *Compiler) compileContinue() {
	if len(c.loopStack) == 0 {
		return
	}
	lbl := c.loopStack[len(c.loopStack)-1]
	if lbl.cont != "" {
		c.writeln(lbl.cont + ".break()")
	}
}

// New creates a new Scala compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), paramAlias: make(map[string]string), pyModules: map[string]string{}}
}

// Compile generates Scala code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	if prog.Package != "" {
		c.writeln("package " + sanitizeName(prog.Package))
		c.writeln("")
	}
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("object Main {")
	c.indent++
	for _, s := range prog.Statements {
		switch {
		case s.Fun != nil:
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		case s.Test != nil:
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		case s.Type == nil:
			c.mainStmts = append(c.mainStmts, s)
		}
	}
	c.writeln("def main(args: Array[String]): Unit = {")
	c.indent++
	for _, s := range c.mainStmts {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln(fmt.Sprintf("%s()", name))
		}
	}
	c.indent--
	c.writeln("}")
	c.emitRuntime()
	c.indent--
	c.writeln("}")
	return FormatScala(c.buf.Bytes()), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	origRet := c.retType
	if fn.Return != nil {
		c.retType = c.resolveTypeRef(fn.Return)
	} else {
		c.retType = types.VoidType{}
	}
	origEnv := c.env
	child := types.NewEnv(c.env)
	for _, p := range fn.Params {
		child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
	}
	c.env = child
	c.writeIndent()
	c.buf.WriteString("def " + sanitizeName(fn.Name) + "(")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(fmt.Sprintf("%s: %s", sanitizeName(p.Name), scalaType(c.resolveTypeRef(p.Type))))
	}
	c.buf.WriteString(")")
	if fn.Return != nil {
		c.buf.WriteString(": " + scalaType(c.resolveTypeRef(fn.Return)))
	}
	c.buf.WriteString(" = {\n")
	c.indent++
	origAlias := c.paramAlias
	c.paramAlias = make(map[string]string)
	for _, p := range fn.Params {
		if paramMutated(fn.Body, p.Name) {
			alias := sanitizeName(p.Name) + "_var"
			c.paramAlias[p.Name] = alias
			typ := scalaType(c.resolveTypeRef(p.Type))
			c.writeln(fmt.Sprintf("var %s: %s = %s", alias, typ, sanitizeName(p.Name)))
		}
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			c.paramAlias = origAlias
			return err
		}
	}
	c.paramAlias = origAlias
	c.env = origEnv
	c.indent--
	c.writeln("}")
	c.retType = origRet
	return nil
}

func (c *Compiler) compileMethod(fn *parser.FunStmt) error {
	origRet := c.retType
	if fn.Return != nil {
		c.retType = c.resolveTypeRef(fn.Return)
	} else {
		c.retType = types.VoidType{}
	}
	origEnv := c.env
	child := types.NewEnv(c.env)
	for _, p := range fn.Params {
		child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
	}
	c.env = child
	c.writeIndent()
	c.buf.WriteString("def " + sanitizeName(fn.Name) + "(")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(fmt.Sprintf("%s: %s", sanitizeName(p.Name), scalaType(c.resolveTypeRef(p.Type))))
	}
	c.buf.WriteString(")")
	if fn.Return != nil {
		c.buf.WriteString(": " + scalaType(c.resolveTypeRef(fn.Return)))
	}
	c.buf.WriteString(" = {\n")
	c.indent++
	origAlias := c.paramAlias
	c.paramAlias = make(map[string]string)
	for _, p := range fn.Params {
		if paramMutated(fn.Body, p.Name) {
			alias := sanitizeName(p.Name) + "_var"
			c.paramAlias[p.Name] = alias
			typ := scalaType(c.resolveTypeRef(p.Type))
			c.writeln(fmt.Sprintf("var %s: %s = %s", alias, typ, sanitizeName(p.Name)))
		}
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			c.paramAlias = origAlias
			return err
		}
	}
	c.paramAlias = origAlias
	c.env = origEnv
	c.indent--
	c.writeln("}")
	c.retType = origRet
	return nil
}

func paramMutated(body []*parser.Statement, name string) bool {
	for _, st := range body {
		if st.Assign != nil && st.Assign.Name == name && len(st.Assign.Index) == 0 {
			return true
		}
		if st.If != nil {
			if paramMutated(st.If.Then, name) || paramMutated(st.If.Else, name) || (st.If.ElseIf != nil && paramMutated([]*parser.Statement{&parser.Statement{If: st.If.ElseIf}}, name)) {
				return true
			}
		}
		if st.For != nil {
			if paramMutated(st.For.Body, name) {
				return true
			}
		}
		if st.While != nil {
			if paramMutated(st.While.Body, name) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.ExternVar != nil:
		return c.compileExternVar(s.ExternVar)
	case s.ExternFun != nil:
		return c.compileExternFun(s.ExternFun)
	case s.ExternType != nil:
		return c.compileExternType(s.ExternType)
	case s.ExternObject != nil:
		return c.compileExternObject(s.ExternObject)
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
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
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.compileBreak()
		return nil
	case s.Continue != nil:
		c.compileContinue()
		return nil
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Import != nil:
		return c.addImport(s.Import)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if expr != "" {
			c.writeln(expr)
		}
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(st *parser.LetStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}

	var t types.Type = types.AnyType{}
	if st.Type != nil {
		t = c.resolveTypeRef(st.Type)
	} else if st.Value != nil {
		t = c.exprType(st.Value)
	}

	typ := scalaType(t)
	name := sanitizeName(st.Name)
	if expr != "" && typ != "Any" {
		et := c.exprType(st.Value)
		if !equalTypes(t, et) || isAny(et) {
			c.use("_cast")
			expr = fmt.Sprintf("_cast[%s](%s)", typ, expr)
		}
	}
	if expr == "" {
		if typ != "Any" {
			c.writeln(fmt.Sprintf("val %s: %s", name, typ))
		} else {
			c.writeln(fmt.Sprintf("val %s", name))
		}
	} else {
		if typ != "Any" {
			c.writeln(fmt.Sprintf("val %s: %s = %s", name, typ, expr))
		} else {
			c.writeln(fmt.Sprintf("val %s = %s", name, expr))
		}
	}
	if c.env != nil {
		c.env.SetVar(st.Name, t, false)
	}
	return nil
}

func (c *Compiler) compileVar(st *parser.VarStmt) error {
	var t types.Type = types.AnyType{}
	if st.Type != nil {
		t = c.resolveTypeRef(st.Type)
	} else if st.Value != nil {
		t = c.exprType(st.Value)
	}

	typ := scalaType(t)

	value := ""
	if st.Value != nil {
		v, err := c.compileExpr(st.Value)
		if err != nil {
			return err
		}
		et := c.exprType(st.Value)
		if typ != "Any" && (!equalTypes(t, et) || isAny(et)) {
			c.use("_cast")
			v = fmt.Sprintf("_cast[%s](%s)", typ, v)
		}
		value = " = " + v
	}

	if st.Value != nil && emptyListExpr(st.Value) {
		if typ == "Any" {
			if lt, ok := c.retType.(types.ListType); ok {
				typ = scalaType(lt)
				t = lt
			} else {
				typ = "scala.collection.mutable.ArrayBuffer[Any]"
			}
		}
		value = " = scala.collection.mutable.ArrayBuffer()"
	}

	name := sanitizeName(st.Name)
	if typ != "Any" && !strings.HasPrefix(typ, "scala.collection.mutable.ArrayBuffer") && !strings.HasPrefix(typ, "scala.collection.mutable.Map") && !strings.HasPrefix(typ, "(") {
		c.writeln(fmt.Sprintf("var %s: %s%s", name, typ, value))
	} else {
		if typ != "Any" && value == "" {
			c.writeln(fmt.Sprintf("var %s: %s", name, typ))
		} else {
			c.writeln(fmt.Sprintf("var %s%s", name, value))
		}
	}
	if c.env != nil {
		c.env.SetVar(st.Name, t, true)
	}
	return nil
}

func (c *Compiler) compileExternVar(ev *parser.ExternVarDecl) error {
	name := sanitizeName(ev.Name())
	typ := scalaType(c.resolveTypeRef(ev.Type))
	c.writeln(fmt.Sprintf("var %s: %s = null.asInstanceOf[%s]", name, typ, typ))
	if c.env != nil {
		c.env.SetVar(ev.Name(), c.resolveTypeRef(ev.Type), true)
	}
	return nil
}

func (c *Compiler) compileExternFun(ef *parser.ExternFunDecl) error {
	params := make([]string, len(ef.Params))
	var ptypes []types.Type
	for i, p := range ef.Params {
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), scalaType(c.resolveTypeRef(p.Type)))
		if c.env != nil {
			ptypes = append(ptypes, c.resolveTypeRef(p.Type))
		}
	}
	ret := scalaType(c.resolveTypeRef(ef.Return))
	c.writeln(fmt.Sprintf("def %s(%s): %s = throw new RuntimeException(\"extern\")", sanitizeName(ef.Name()), strings.Join(params, ", "), ret))
	if c.env != nil {
		ft := types.FuncType{Params: ptypes, Return: c.resolveTypeRef(ef.Return)}
		c.env.SetVar(ef.Name(), ft, false)
	}
	return nil
}

func (c *Compiler) compileExternType(et *parser.ExternTypeDecl) error {
	c.writeln(fmt.Sprintf("type %s = Any", sanitizeName(et.Name)))
	return nil
}

func (c *Compiler) compileExternObject(eo *parser.ExternObjectDecl) error {
	name := sanitizeName(eo.Name)
	c.use("_extern")
	c.writeln(fmt.Sprintf("val %s = ExternRegistry._externGet(\"%s\")", name, eo.Name))
	if c.env != nil {
		c.env.SetVar(eo.Name, types.AnyType{}, true)
	}
	return nil
}

func (c *Compiler) compileAssign(st *parser.AssignStmt) error {
	lhs := sanitizeName(st.Name)
	if alias, ok := c.paramAlias[st.Name]; ok {
		lhs = alias
	}
	if len(st.Index) == 0 {
		rhs, err := c.compileExpr(st.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s", lhs, rhs))
		return nil
	}

	idx, err := c.compileExpr(st.Index[0].Start)
	if err != nil {
		return err
	}
	rhs, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s.update(%s, %s)", lhs, idx, rhs))
	return nil
}

func (c *Compiler) compileReturn(st *parser.ReturnStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	if c.retType != nil {
		et := c.exprType(st.Value)
		if !equalTypes(c.retType, et) || isAny(et) {
			typ := scalaType(c.retType)
			c.use("_cast")
			expr = fmt.Sprintf("_cast[%s](%s)", typ, expr)
		}
	}
	c.writeln("return " + expr)
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("def " + name + "(): Unit = {\n")
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
	c.use("_expect")
	c.writeln(fmt.Sprintf("expect(%s)", expr))
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	needBreak := containsBreak(st.Body)
	needCont := containsContinue(st.Body)
	lbl := loopLabels{}
	if needBreak || needCont {
		lbl.brk = c.newTemp("brk")
		c.writeln(fmt.Sprintf("val %s = new scala.util.control.Breaks", lbl.brk))
		c.writeln(fmt.Sprintf("%s.breakable {", lbl.brk))
		c.indent++
	}
	c.loopStack = append(c.loopStack, lbl)
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent++
	if needCont {
		lbl.cont = c.newTemp("cont")
		c.writeln(fmt.Sprintf("val %s = new scala.util.control.Breaks", lbl.cont))
		c.writeln(fmt.Sprintf("%s.breakable {", lbl.cont))
		c.indent++
		c.loopStack[len(c.loopStack)-1] = lbl
	}
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			return err
		}
	}
	if needCont {
		c.indent--
		c.writeln("}")
	}
	c.indent--
	c.writeln("}")
	c.loopStack = c.loopStack[:len(c.loopStack)-1]
	if needBreak || needCont {
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileFor(st *parser.ForStmt) error {
	name := sanitizeName(st.Name)
	needBreak := containsBreak(st.Body)
	needCont := containsContinue(st.Body)
	lbl := loopLabels{}
	useVar := st.Name != "_"
	var elemType types.Type = types.AnyType{}
	if needBreak || needCont {
		lbl.brk = c.newTemp("brk")
		c.writeln(fmt.Sprintf("val %s = new scala.util.control.Breaks", lbl.brk))
		c.writeln(fmt.Sprintf("%s.breakable {", lbl.brk))
		c.indent++
	}

	c.loopStack = append(c.loopStack, lbl)
	oldEnv := c.env
	if c.env != nil {
		c.env = types.NewEnv(c.env)
	}
	if st.RangeEnd != nil {
		start, err := c.compileExpr(st.Source)
		if err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			return err
		}
		end, err := c.compileExpr(st.RangeEnd)
		if err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			return err
		}
		if _, ok := c.exprType(st.Source).(types.Int64Type); ok {
			elemType = types.Int64Type{}
		} else {
			elemType = types.IntType{}
		}
		if useVar && c.env != nil {
			c.env.SetVar(st.Name, elemType, true)
		}
		idx := c.newTemp("i")
		c.writeln(fmt.Sprintf("var %s = %s", idx, start))
		c.writeln(fmt.Sprintf("while (%s < %s) {", idx, end))
		c.indent++
		typ := scalaType(elemType)
		if useVar && typ != "Any" {
			c.writeln(fmt.Sprintf("val %s: %s = %s", name, typ, idx))
		} else {
			c.writeln(fmt.Sprintf("val %s = %s", name, idx))
		}
		c.writeln(fmt.Sprintf("%s = %s + 1", idx, idx))
	} else {
		src, err := c.compileExpr(st.Source)
		if err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			return err
		}
		srcType := c.exprType(st.Source)
		switch tt := srcType.(type) {
		case types.ListType:
			elemType = tt.Elem
		case types.MapType:
			elemType = tt.Key
		case types.StringType:
			elemType = types.StringType{}
		}
		if useVar && c.env != nil {
			c.env.SetVar(st.Name, elemType, true)
		}
		it := c.newTemp("it")
		if _, ok := srcType.(types.MapType); ok {
			c.writeln(fmt.Sprintf("val %s = %s.keysIterator", it, src))
		} else {
			c.writeln(fmt.Sprintf("val %s = %s.iterator", it, src))
		}
		c.writeln(fmt.Sprintf("while (%s.hasNext) {", it))
		c.indent++
		typ := scalaType(elemType)
		next := fmt.Sprintf("%s.next()", it)
		if typ == "String" {
			next += ".toString"
		}
		if useVar && typ != "Any" {
			c.writeln(fmt.Sprintf("val %s: %s = %s", name, typ, next))
		} else {
			c.writeln(fmt.Sprintf("val %s = %s", name, next))
		}
	}

	if needCont {
		lbl.cont = c.newTemp("cont")
		c.writeln(fmt.Sprintf("val %s = new scala.util.control.Breaks", lbl.cont))
		c.writeln(fmt.Sprintf("%s.breakable {", lbl.cont))
		c.indent++
		c.loopStack[len(c.loopStack)-1] = lbl
	}

	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			if c.env != nil {
				c.env = oldEnv
			}
			return err
		}
	}

	if needCont {
		c.indent--
		c.writeln("}")
	}
	c.indent--
	c.writeln("}")
	c.loopStack = c.loopStack[:len(c.loopStack)-1]
	if needBreak || needCont {
		c.indent--
		c.writeln("}")
	}
	if c.env != nil {
		c.env = oldEnv
	}
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	target := sanitizeName(u.Target)
	if alias, ok := c.paramAlias[u.Target]; ok {
		target = alias
	}

	idx := c.newTemp("i")
	c.writeln(fmt.Sprintf("var %s = 0", idx))
	c.writeln(fmt.Sprintf("while (%s < %s.length) {", idx, target))
	c.indent++

	item := c.newTemp("it")
	c.use("_indexList")
	c.writeln(fmt.Sprintf("var %s = _indexList(%s, %s)", item, target, idx))

	origEnv := c.env
	if c.env != nil {
		child := types.NewEnv(c.env)
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if st, ok := lt.Elem.(types.StructType); ok {
					for _, f := range st.Order {
						child.SetVar(f, st.Fields[f], true)
						typ := scalaType(st.Fields[f])
						if typ != "Any" {
							c.writeln(fmt.Sprintf("val %s: %s = %s.%s", sanitizeName(f), typ, item, sanitizeName(f)))
						} else {
							c.writeln(fmt.Sprintf("val %s = %s.%s", sanitizeName(f), item, sanitizeName(f)))
						}
					}
				}
			}
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

	updates := make([]string, len(u.Set.Items))
	for i, it := range u.Set.Items {
		keyStr, _ := identName(it.Key)
		val, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		updates[i] = fmt.Sprintf("%s = %s", sanitizeName(keyStr), val)
	}
	if len(updates) > 0 {
		c.writeln(fmt.Sprintf("%s = %s.copy(%s)", item, item, strings.Join(updates, ", ")))
	}

	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	c.writeln(fmt.Sprintf("%s.update(%s, %s)", target, idx, item))
	c.writeln(fmt.Sprintf("%s = %s + 1", idx, idx))

	if c.env != nil {
		c.env = origEnv
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeIndent()
	c.buf.WriteString("if (" + cond + ") {\n")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if st.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(st.ElseIf)
	}
	if len(st.Else) > 0 {
		c.buf.WriteString(" else {\n")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}")
	}
	c.buf.WriteByte('\n')
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

	ops := []string{}
	operands := []string{left}
	nodes := []*parser.PostfixExpr{b.Left.Value}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		nodes = append(nodes, part.Right)
		ops = append(ops, part.Op)
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

			l := operands[i]
			r := operands[i+1]
			op := ops[i]

			var expr string
			switch op {
			case "in":
				expr = fmt.Sprintf("%s.contains(%s)", r, l)
			case "+":
				if nodes[i+1].Target.List != nil {
					expr = fmt.Sprintf("(%s ++ %s)", l, r)
				} else {
					expr = fmt.Sprintf("(%s + %s)", l, r)
				}
			case "union_all":
				expr = fmt.Sprintf("(%s ++ %s)", l, r)
			case "union":
				expr = fmt.Sprintf("(%s ++ %s).distinct", l, r)
			case "except":
				expr = fmt.Sprintf("%s.filterNot(%s.contains)", l, r)
			case "intersect":
				expr = fmt.Sprintf("%s.filter(%s.contains).distinct", l, r)
			default:
				expr = fmt.Sprintf("(%s %s %s)", l, op, r)
			}

			operands[i] = expr
			operands = append(operands[:i+1], operands[i+2:]...)
			nodes = append(nodes[:i+1], nodes[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state after binary compilation")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = fmt.Sprintf("(%s%s)", u.Ops[i], expr)
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	if sel := p.Target.Selector; sel != nil {
		if mod, ok := c.pyModules[sel.Root]; ok {
			attr := strings.Join(sel.Tail, ".")
			if len(p.Ops) == 0 {
				c.use("_pyAttr")
				return fmt.Sprintf("_pyAttr(%q, %q, Seq())", mod, attr), nil
			}
			if len(p.Ops) == 1 && p.Ops[0].Call != nil {
				args := make([]string, len(p.Ops[0].Call.Args))
				for i, a := range p.Ops[0].Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				c.use("_pyAttr")
				return fmt.Sprintf("_pyAttr(%q, %q, Seq(%s))", mod, attr, strings.Join(args, ", ")), nil
			}
			return "", fmt.Errorf("unsupported FFI expression")
		}
	}
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("%s.length", expr)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if isStringPrimary(p.Target, c.env) {
					c.use("_sliceString")
					expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, end)
				} else if isListPrimary(p.Target, c.env) {
					c.use("_slice")
					expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, start, end)
				} else {
					expr = fmt.Sprintf("%s.slice(%s, %s)", expr, start, end)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if isStringPrimary(p.Target, c.env) {
					c.use("_indexString")
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, idx)
				} else if isListPrimary(p.Target, c.env) {
					c.use("_indexList")
					expr = fmt.Sprintf("_indexList(%s, %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("%s(%s)", expr, idx)
				}
			}
			continue
		}
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
			continue
		}
		if op.Cast != nil {
			typ := scalaType(c.resolveTypeRef(op.Cast.Type))
			c.use("_cast")
			expr = fmt.Sprintf("_cast[%s](%s)", typ, expr)
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s = %s", sanitizeName(f.Name), v)
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
		if p.Lit.Null {
			return "null", nil
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
		return "scala.collection.mutable.ArrayBuffer(" + strings.Join(elems, ", ") + ")", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			if s, ok := simpleStringKey(it.Key); ok {
				k = fmt.Sprintf("%q", s)
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s -> %s", k, v)
		}
		return "scala.collection.mutable.Map(" + strings.Join(items, ", ") + ")", nil
	case p.Selector != nil:
		var expr string
		if alias, ok := c.paramAlias[p.Selector.Root]; ok {
			expr = alias
		} else {
			expr = sanitizeName(p.Selector.Root)
		}
		isMap := isMapVar(p.Selector.Root, c.env)
		for i, t := range p.Selector.Tail {
			if isMap && i == 0 {
				expr = fmt.Sprintf("%s(%q)", expr, t)
			} else {
				expr = expr + "." + sanitizeName(t)
			}
		}
		return expr, nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Call != nil:
		return c.compileCall(p.Call, "")
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileCall(call *parser.CallExpr, recv string) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	argStr := strings.Join(args, ", ")
	if recv != "" {
		// method call is not used yet
	}
	switch call.Func {
	case "print":
		return fmt.Sprintf("println(%s)", argStr), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		if isMapExpr(call.Args[0], c.env) {
			return fmt.Sprintf("%s.size", args[0]), nil
		}
		return fmt.Sprintf("%s.length", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("%s.size", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		a := args[0]
		return fmt.Sprintf("(%s.sum / %s.size)", a, a), nil
	case "input":
		if len(args) != 0 {
			return "", fmt.Errorf("input expects no args")
		}
		return "scala.io.StdIn.readLine()", nil
	case "str":
		if len(args) == 1 {
			return args[0] + ".toString()", nil
		}
	case "int":
		if len(args) != 1 {
			return "", fmt.Errorf("int expects 1 arg")
		}
		c.use("_cast")
		return fmt.Sprintf("_cast[Int](%s)", args[0]), nil
	case "float":
		if len(args) != 1 {
			return "", fmt.Errorf("float expects 1 arg")
		}
		c.use("_cast")
		return fmt.Sprintf("_cast[Double](%s)", args[0]), nil
	case "bool":
		if len(args) != 1 {
			return "", fmt.Errorf("bool expects 1 arg")
		}
		c.use("_cast")
		return fmt.Sprintf("_cast[Boolean](%s)", args[0]), nil
	case "now":
		if len(args) != 0 {
			return "", fmt.Errorf("now expects no args")
		}
		return "System.currentTimeMillis() * 1000000L", nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		c.use("_to_json")
		c.use("_json")
		return fmt.Sprintf("_json(%s)", args[0]), nil
	case "to_json":
		if len(args) != 1 {
			return "", fmt.Errorf("to_json expects 1 arg")
		}
		c.use("_to_json")
		return fmt.Sprintf("_to_json(%s)", args[0]), nil
	case "reduce":
		if len(args) != 3 {
			return "", fmt.Errorf("reduce expects 3 args")
		}
		c.use("_reduce")
		return fmt.Sprintf("_reduce(%s, %s, %s)", args[0], args[1], args[2]), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("%s.append(%s)", args[0], args[1]), nil
	case "concat":
		if len(args) == 0 {
			return "scala.collection.mutable.ArrayBuffer()", nil
		}
		expr := args[0]
		for _, a := range args[1:] {
			expr = fmt.Sprintf("%s ++ %s", expr, a)
		}
		return expr, nil
	case "substr", "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substr expects 3 args")
		}
		if isStringExpr(call.Args[0], c.env) {
			c.use("_sliceString")
			return fmt.Sprintf("_sliceString(%s, %s, %s)", args[0], args[1], args[2]), nil
		}
		c.use("_slice")
		return fmt.Sprintf("_slice(%s, %s, %s)", args[0], args[1], args[2]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		c.use("_exists")
		return fmt.Sprintf("_exists(%s)", args[0]), nil
	case "contains":
		if len(args) != 2 {
			return "", fmt.Errorf("contains expects 2 args")
		}
		return fmt.Sprintf("%s.contains(%s)", args[0], args[1]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("%s.values.toSeq", args[0]), nil
	case "strings.ToUpper":
		if len(args) != 1 {
			return "", fmt.Errorf("strings.ToUpper expects 1 arg")
		}
		return fmt.Sprintf("%s.toUpperCase()", args[0]), nil
	case "eval":
		if len(args) != 1 {
			return "", fmt.Errorf("eval expects 1 arg")
		}
		c.use("_eval")
		return fmt.Sprintf("_eval(%s)", args[0]), nil
	}
	return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := scalaType(c.resolveTypeRef(p.Type))
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), typ)
	}

	origEnv := c.env
	child := types.NewEnv(c.env)
	for _, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		}
	}
	c.env = child

	var body string
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		body = expr
	} else {
		var buf bytes.Buffer
		savedBuf := c.buf
		savedIndent := c.indent
		c.buf = buf
		c.indent = 0
		origAlias := c.paramAlias
		c.paramAlias = make(map[string]string)
		for _, p := range fn.Params {
			if paramMutated(fn.BlockBody, p.Name) {
				alias := sanitizeName(p.Name) + "_var"
				c.paramAlias[p.Name] = alias
				typ := scalaType(c.resolveTypeRef(p.Type))
				c.writeln(fmt.Sprintf("var %s: %s = %s", alias, typ, sanitizeName(p.Name)))
			}
		}
		for _, st := range fn.BlockBody {
			if err := c.compileStmt(st); err != nil {
				c.env = origEnv
				c.buf = savedBuf
				c.indent = savedIndent
				c.paramAlias = origAlias
				return "", err
			}
		}
		body = "{\n" + indentBlock(c.buf.String(), 1) + "}"
		c.buf = savedBuf
		c.indent = savedIndent
		c.paramAlias = origAlias
	}
	c.env = origEnv

	return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), body), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	orig := c.env
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, types.AnyType{}, true)

	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
		child.SetVar(f.Var, types.AnyType{}, true)
	}

	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	joinSides := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		child.SetVar(j.Var, types.AnyType{}, true)
	}

	c.env = child

	for i, j := range q.Joins {
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinOns[i] = on
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
	}

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

	if q.Group != nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}

		varNames := []string{sanitizeName(q.Var)}
		for _, f := range q.Froms {
			varNames = append(varNames, sanitizeName(f.Var))
		}
		params := append([]string(nil), varNames...)
		joins := make([]string, 0, len(q.Froms)+len(q.Joins))
		for _, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("Map(\"items\" -> %s)", fs))
		}
		for i, js := range joinSrcs {
			onParams := append(params, sanitizeName(q.Joins[i].Var))
			onFn := seqLambda(onParams, joinOns[i])
			spec := fmt.Sprintf("Map(\"items\" -> %s, \"on\" -> %s", js, onFn)
			if joinSides[i] == "left" || joinSides[i] == "outer" {
				spec += ", \"left\" -> true"
			}
			if joinSides[i] == "right" || joinSides[i] == "outer" {
				spec += ", \"right\" -> true"
			}
			spec += ")"
			joins = append(joins, spec)
			params = append(params, sanitizeName(q.Joins[i].Var))
		}

		innerSel := seqLambda(varNames, sanitizeName(q.Var))
		var inner strings.Builder
		inner.WriteString("(() => {\n")
		inner.WriteString(fmt.Sprintf("\tval src = %s\n", src))
		inner.WriteString("\tval res = _query(src, Seq(\n")
		for i, j := range joins {
			inner.WriteString("\t\t" + j)
			if i != len(joins)-1 {
				inner.WriteString(",")
			}
			inner.WriteString("\n")
		}
		inner.WriteString("\t), Map(\"select\" -> " + innerSel)
		if cond != "" {
			inner.WriteString(", \"where\" -> " + seqLambda(params, cond))
		}
		if sortExpr != "" {
			inner.WriteString(", \"sortKey\" -> " + seqLambda(params, sortExpr))
		}
		if skipExpr != "" {
			inner.WriteString(", \"skip\" -> " + skipExpr)
		}
		if takeExpr != "" {
			inner.WriteString(", \"take\" -> " + takeExpr)
		}
		inner.WriteString("))\n")
		inner.WriteString("\tres\n")
		inner.WriteString("})()")
		innerQuery := inner.String()

		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig
		c.use("_Group")
		c.use("_group_by")
		c.use("_query")
		expr := fmt.Sprintf("_group_by(%s, (%s: Any) => %s).map(%s => %s).toSeq", innerQuery, sanitizeName(q.Var), keyExpr, sanitizeName(q.Group.Name), valExpr)
		return expr, nil
	}

	c.env = orig

	varNames := []string{sanitizeName(q.Var)}
	for _, f := range q.Froms {
		varNames = append(varNames, sanitizeName(f.Var))
	}
	params := append([]string(nil), varNames...)
	joins := make([]string, 0, len(q.Froms)+len(q.Joins))
	for _, fs := range fromSrcs {
		joins = append(joins, fmt.Sprintf("Map(\"items\" -> %s)", fs))
	}
	for i, js := range joinSrcs {
		onParams := append(params, sanitizeName(q.Joins[i].Var))
		onFn := seqLambda(onParams, joinOns[i])
		spec := fmt.Sprintf("Map(\"items\" -> %s, \"on\" -> %s", js, onFn)
		if joinSides[i] == "left" || joinSides[i] == "outer" {
			spec += ", \"left\" -> true"
		}
		if joinSides[i] == "right" || joinSides[i] == "outer" {
			spec += ", \"right\" -> true"
		}
		spec += ")"
		joins = append(joins, spec)
		params = append(params, sanitizeName(q.Joins[i].Var))
	}

	selectFn := seqLambda(params, sel)
	var whereFn, sortFn string
	if cond != "" {
		whereFn = seqLambda(params, cond)
	}
	if sortExpr != "" {
		sortFn = seqLambda(params, sortExpr)
	}

	c.use("_query")
	var b strings.Builder
	b.WriteString("(() => {\n")
	b.WriteString(fmt.Sprintf("\tval src = %s\n", src))
	b.WriteString("\tval res = _query(src, Seq(\n")
	for i, j := range joins {
		b.WriteString("\t\t" + j)
		if i != len(joins)-1 {
			b.WriteString(",")
		}
		b.WriteString("\n")
	}
	b.WriteString("\t), Map(\"select\" -> " + selectFn)
	if whereFn != "" {
		b.WriteString(", \"where\" -> " + whereFn)
	}
	if sortFn != "" {
		b.WriteString(", \"sortKey\" -> " + sortFn)
	}
	if skipExpr != "" {
		b.WriteString(", \"skip\" -> " + skipExpr)
	}
	if takeExpr != "" {
		b.WriteString(", \"take\" -> " + takeExpr)
	}
	b.WriteString("))\n")
	b.WriteString("\tres\n")
	b.WriteString("})()")
	return b.String(), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "Map[String, Any]()"
	if f.With != nil {
		o, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", url, opts), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "Map[String, Any]()"
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
		typ := scalaType(c.resolveTypeRef(l.Type))
		if typ != "" {
			expr = fmt.Sprintf("_load(%s, %s).map(_.asInstanceOf[%s])", path, opts, typ)
		}
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
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "Map[String, Any]()"
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
			params = append(params, fmt.Sprintf("%q -> %s", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramMap := "Map[String, Any]()"
	if len(params) > 0 {
		paramMap = "Map(" + strings.Join(params, ", ") + ")"
	}
	if model == "" {
		model = "\"\""
	}
	if g.Target == "embedding" {
		c.use("_genEmbed")
		return fmt.Sprintf("_genEmbed(%s, %s, %s)", text, model, paramMap), nil
	}
	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_genStruct")
			return fmt.Sprintf("_genStruct[%s](%s, %s, %s)", sanitizeName(g.Target), prompt, model, paramMap), nil
		}
	}
	c.use("_genText")
	return fmt.Sprintf("_genText(%s, %s, %s)", prompt, model, paramMap), nil
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "bool":
			return types.BoolType{}
		case "string":
			return types.StringType{}
		default:
			if c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
			}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return types.ListType{Elem: c.resolveTypeRef(t.Generic.Args[0])}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return types.MapType{Key: c.resolveTypeRef(t.Generic.Args[0]), Value: c.resolveTypeRef(t.Generic.Args[1])}
		}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	return types.AnyType{}
}

func scalaType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.BoolType:
		return "Boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		return "scala.collection.mutable.ArrayBuffer[" + scalaType(tt.Elem) + "]"
	case types.MapType:
		return "scala.collection.mutable.Map[" + scalaType(tt.Key) + ", " + scalaType(tt.Value) + "]"
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = scalaType(p)
		}
		ret := "Unit"
		if tt.Return != nil {
			ret = scalaType(tt.Return)
		}
		if len(params) == 1 {
			return fmt.Sprintf("(%s => %s)", params[0], ret)
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), ret)
	default:
		return "Any"
	}
}

var scalaKeywords = map[string]struct{}{
	"abstract": {}, "case": {}, "catch": {}, "class": {}, "def": {}, "do": {},
	"else": {}, "extends": {}, "false": {}, "final": {}, "finally": {},
	"for": {}, "if": {}, "implicit": {}, "import": {}, "lazy": {}, "match": {},
	"new": {}, "null": {}, "object": {}, "override": {}, "package": {},
	"private": {}, "protected": {}, "return": {}, "sealed": {}, "super": {},
	"this": {}, "throw": {}, "trait": {}, "try": {}, "true": {}, "type": {},
	"val": {}, "var": {}, "while": {}, "with": {}, "yield": {},
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z') || ('0' <= r && r <= '9' && i > 0) {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" {
		return "_"
	}
	if !(s[0] >= 'a' && s[0] <= 'z' || s[0] >= 'A' && s[0] <= 'Z' || s[0] == '_') {
		s = "_" + s
	}
	if _, ok := scalaKeywords[s]; ok {
		s = "_" + s
	}
	return s
}

func joinArgs(args []string) string {
	if len(args) == 0 {
		return ""
	}
	res := args[0]
	for i := 1; i < len(args); i++ {
		res += ", " + args[i]
	}
	return res
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
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "null"
	}
	return fmt.Sprintf("(if (%s) %s else %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if len(t.Variants) > 0 {
		c.writeln("sealed trait " + name)
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				typ := scalaType(c.resolveTypeRef(f.Type))
				fields[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), typ)
			}
			params := strings.Join(fields, ", ")
			c.writeln(fmt.Sprintf("case class %s(%s) extends %s", vname, params, name))
		}
		return nil
	}
	fields := []string{}
	var methods []*parser.FunStmt
	for _, m := range t.Members {
		if m.Field != nil {
			typ := scalaType(c.resolveTypeRef(m.Field.Type))
			fields = append(fields, fmt.Sprintf("%s: %s", sanitizeName(m.Field.Name), typ))
		} else if m.Method != nil {
			methods = append(methods, m.Method)
		}
	}
	if len(methods) == 0 {
		c.writeln(fmt.Sprintf("case class %s(%s)", name, strings.Join(fields, ", ")))
		return nil
	}
	c.writeln(fmt.Sprintf("case class %s(%s) {", name, strings.Join(fields, ", ")))
	c.indent++
	for i, m := range methods {
		if err := c.compileMethod(m); err != nil {
			return err
		}
		if i < len(methods)-1 {
			c.writeln("")
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(" + target + " match {\n")
	for _, cs := range m.Cases {
		pat, err := c.compileMatchPattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		b.WriteString("\tcase " + pat + " => " + res + "\n")
	}
	b.WriteString("})")
	return b.String(), nil
}

func (c *Compiler) compileMatchPattern(pat *parser.Expr) (string, error) {
	if isUnderscoreExpr(pat) {
		return "_", nil
	}
	if call, ok := callPattern(pat); ok {
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			if name, ok := identName(a); ok {
				args[i] = sanitizeName(name)
			} else {
				return "", fmt.Errorf("invalid pattern")
			}
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), strings.Join(args, ", ")), nil
	}
	if id, ok := identName(pat); ok {
		return sanitizeName(id), nil
	}
	return c.compileExpr(pat)
}

func (c *Compiler) addImport(im *parser.ImportStmt) error {
	if im.Lang == nil || *im.Lang != "python" {
		if im.Lang == nil {
			return fmt.Errorf("unsupported import language: <nil>")
		}
		return fmt.Errorf("unsupported import language: %s", *im.Lang)
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	if c.pyModules == nil {
		c.pyModules = map[string]string{}
	}
	c.pyModules[alias] = strings.Trim(im.Path, "\"")
	return nil
}
