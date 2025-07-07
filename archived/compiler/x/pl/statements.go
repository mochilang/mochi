//go:build archived

package plcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// compileBlock compiles a sequence of statements at one indentation level and
// trims the trailing comma if present. The current indentation is preserved and
// the generated code is returned.
func (c *Compiler) compileBlock(stmts []*parser.Statement, ret string) ([]byte, error) {
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = oldIndent + 1
	for _, s := range stmts {
		if err := c.compileStmt(s, ret); err != nil {
			c.buf = oldBuf
			c.indent = oldIndent
			return nil, err
		}
	}
	b := c.buf.Bytes()
	if bytes.HasSuffix(b, []byte(",\n")) {
		b = b[:len(b)-2]
		b = append(b, '\n')
	}
	c.buf = oldBuf
	c.indent = oldIndent
	return b, nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	name := sanitizeAtom(fn.Name)
	if c.env != nil {
		c.env.SetFunc(fn.Name, fn)
	}
	oldVars := c.vars
	oldFun := c.currFun
	newVars := make(map[string]string)
	for k, v := range c.vars {
		newVars[k] = v
	}
	c.vars = newVars
	c.currFun = name
	defer func() {
		c.vars = oldVars
		c.currFun = oldFun
	}()
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeVar(p.Name)
	}
	ret := "Res"

	stmts := fn.Body
	var fallback *parser.Statement
	if len(stmts) > 0 {
		last := stmts[len(stmts)-1]
		if last.Return != nil {
			fallback = last
			stmts = stmts[:len(stmts)-1]
		}
	}

	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = oldIndent + 2
	for _, s := range stmts {
		if err := c.compileStmt(s, ret); err != nil {
			c.buf = oldBuf
			return err
		}
	}
	b := c.buf.Bytes()
	hasBody := len(b) > 0
	if bytes.HasSuffix(b, []byte(",\n")) {
		b = b[:len(b)-2]
		b = append(b, '\n')
	}
	c.buf = oldBuf
	if hasBody || fallback == nil {
		if len(params) == 0 {
			c.writeln(fmt.Sprintf("%s(%s) :-", name, ret))
		} else {
			c.writeln(fmt.Sprintf("%s(%s, %s) :-", name, strings.Join(params, ", "), ret))
		}
		c.indent++
		c.writeln("catch(")
		c.indent++
		c.writeln("(")
		c.indent++
		c.buf.Write(b)
		if hasBody {
			c.writeln(",")
		}
		c.writeln("true")
		c.indent--
		c.writeln(")")
		handler := c.newVar()
		c.writeln(fmt.Sprintf(", return(%s),", handler))
		c.indent++
		c.writeln(fmt.Sprintf("%s = %s", ret, handler))
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(".")
	}
	if fallback != nil {
		val, err := c.compileExpr(fallback.Return.Value)
		if err != nil {
			return err
		}
		if len(params) == 0 {
			c.writeln(fmt.Sprintf("%s(%s) :-", name, ret))
		} else {
			c.writeln(fmt.Sprintf("%s(%s, %s) :-", name, strings.Join(params, ", "), ret))
		}
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("%s = %s.", ret, val.val))
	}
	c.indent = oldIndent
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement, ret string) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		name := sanitizeVar(s.Let.Name)
		t := c.inferExprType(s.Let.Value)
		c.writeln(fmt.Sprintf("%% %s :: %s", name, t.String()))
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("%s = %s,", name, val.val))
		if fn := pureFunExpr(s.Let.Value); fn != nil {
			c.funVars[s.Let.Name] = val.val
		}
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Import != nil:
		return nil
	case s.While != nil:
		return c.compileWhile(s.While, ret)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("throw(return(%s))", val.val))
	case s.Fact != nil:
		return c.compileFact(s.Fact)
	case s.Rule != nil:
		return c.compileRule(s.Rule)
	case s.Expr != nil:
		if call := s.Expr.Expr.Binary.Left.Value.Target.Call; call != nil && call.Func == "print" {
			if len(call.Args) == 0 {
				return fmt.Errorf("print expects at least 1 arg")
			}
			for i, a := range call.Args {
				arg, err := c.compileExpr(a)
				if err != nil {
					return err
				}
				for _, line := range arg.code {
					c.writeln(line)
				}
				c.writeln(fmt.Sprintf("write(%s),", arg.val))
				if i < len(call.Args)-1 {
					c.writeln("write(' '),")
				}
			}
			c.writeln("nl,")
		} else {
			val, err := c.compileExpr(s.Expr.Expr)
			if err != nil {
				return err
			}
			for _, line := range val.code {
				c.writeln(line)
			}
		}
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.For != nil:
		return c.compileFor(s.For, ret)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Break != nil:
		c.writeln("throw(break)")
		return nil
	case s.Continue != nil:
		c.writeln("throw(continue)")
		return nil
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.If != nil:
		return c.compileIf(s.If, ret, true)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt, ret string) error {
	if f.RangeEnd == nil {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		listVar := c.newVar()
		for _, line := range src.code {
			c.writeln(line)
		}
		if types.IsMapType(types.TypeOfExprBasic(f.Source, c.env)) {
			c.use("map_keys")
			c.writeln(fmt.Sprintf("map_keys(%s, %s),", src.val, listVar))
		} else {
			c.use("tolist")
			c.writeln(fmt.Sprintf("to_list(%s, %s),", src.val, listVar))
		}
		loopVar := sanitizeVar(f.Name)
		c.writeln("catch(")
		c.indent++
		c.writeln("(")
		c.indent++
		c.writeln(fmt.Sprintf("member(%s, %s),", loopVar, listVar))
		c.writeln("catch(")
		c.indent++
		c.writeln("(")
		c.indent++
		for _, s := range f.Body {
			if err := c.compileStmt(s, ret); err != nil {
				return err
			}
		}
		c.writeln("true")
		c.indent--
		c.writeln("), continue, true),")
		c.writeln("fail")
		c.writeln(";")
		c.writeln("true")
		c.indent--
		c.writeln(")")
		c.writeln(", break, true),")
		c.writeln("true,")
		c.indent--
		return nil
	}
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end, err := c.compileExpr(f.RangeEnd)
	if err != nil {
		return err
	}
	tempEnd := c.newVar()
	for _, line := range start.code {
		c.writeln(line)
	}
	for _, line := range end.code {
		c.writeln(line)
	}
	c.writeln(fmt.Sprintf("%s is %s - 1,", tempEnd, end.val))
	loopVar := sanitizeVar(f.Name)
	c.writeln("catch(")
	c.indent++
	c.writeln("(")
	c.indent++
	c.writeln(fmt.Sprintf("between(%s, %s, %s),", start.val, tempEnd, loopVar))
	c.writeln("catch(")
	c.indent++
	c.writeln("(")
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s, ret); err != nil {
			return err
		}
	}
	c.writeln("true")
	c.indent--
	c.writeln("), continue, true),")
	c.writeln("fail")
	c.writeln(";")
	c.writeln("true")
	c.indent--
	c.writeln(")")
	c.writeln(", break, true),")
	c.writeln("true,")
	c.indent--
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt, ret string, trailing bool) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	for _, line := range cond.code {
		c.writeln(line)
	}
	c.writeln(fmt.Sprintf("(%s ->", cond.val))

	thenBlock, err := c.compileBlock(stmt.Then, ret)
	if err != nil {
		return err
	}
	c.buf.Write(thenBlock)

	c.writeln(";")

	switch {
	case stmt.ElseIf != nil:
		if err := c.compileIf(stmt.ElseIf, ret, false); err != nil {
			return err
		}
	case len(stmt.Else) > 0:
		elseBlock, err := c.compileBlock(stmt.Else, ret)
		if err != nil {
			return err
		}
		c.buf.Write(elseBlock)
	default:
		c.writeln("true")
	}

	if trailing {
		c.writeln("),")
	} else {
		c.writeln(")")
	}
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	name := sanitizeAtom(v.Name)
	c.vars[v.Name] = name
	if v.Value != nil {
		val, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		t := c.inferExprType(v.Value)
		c.writeln(fmt.Sprintf("%% %s :: %s", name, t.String()))
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", name, val.val))
		if fn := pureFunExpr(v.Value); fn != nil {
			c.funVars[v.Name] = val.val
		}
	} else {
		c.writeln(fmt.Sprintf("nb_setval(%s, _),", name))
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	if name, ok := c.vars[a.Name]; ok && len(a.Index) == 0 {
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return err
		}
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", name, val.val))
		if fn := pureFunExpr(a.Value); fn != nil {
			c.funVars[a.Name] = val.val
		}
		return nil
	}
	if name, ok := c.vars[a.Name]; ok && allSimple(a.Index) {
		idxRes := make([]exprRes, len(a.Index))
		for i, idx := range a.Index {
			r, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			idxRes[i] = r
		}
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return err
		}

		cur := c.newVar()
		c.writeln(fmt.Sprintf("nb_getval(%s, %s),", name, cur))
		containers := []string{cur}
		for _, r := range idxRes[:len(idxRes)-1] {
			for _, line := range r.code {
				c.writeln(line)
			}
			next := c.newVar()
			c.use("getitem")
			c.writeln(fmt.Sprintf("get_item(%s, %s, %s),", cur, r.val, next))
			cur = next
			containers = append(containers, cur)
		}
		for _, line := range idxRes[len(idxRes)-1].code {
			c.writeln(line)
		}
		for _, line := range val.code {
			c.writeln(line)
		}

		newVal := c.newVar()
		c.use("setitem")
		c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", cur, idxRes[len(idxRes)-1].val, val.val, newVal))

		for i := len(containers) - 2; i >= 0; i-- {
			tmp := c.newVar()
			c.use("setitem")
			c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", containers[i], idxRes[i].val, newVal, tmp))
			newVal = tmp
		}
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", name, newVal))
		return nil
	}
	if len(a.Index) == 0 {
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return err
		}
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("%s = %s,", sanitizeVar(a.Name), val.val))
		return nil
	}
	if allSimple(a.Index) {
		idxRes := make([]exprRes, len(a.Index))
		for i, idx := range a.Index {
			r, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			idxRes[i] = r
		}
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return err
		}

		cur := sanitizeVar(a.Name)
		containers := []string{cur}
		for _, r := range idxRes[:len(idxRes)-1] {
			for _, line := range r.code {
				c.writeln(line)
			}
			next := c.newVar()
			c.use("getitem")
			c.writeln(fmt.Sprintf("get_item(%s, %s, %s),", cur, r.val, next))
			cur = next
			containers = append(containers, cur)
		}
		for _, line := range idxRes[len(idxRes)-1].code {
			c.writeln(line)
		}
		for _, line := range val.code {
			c.writeln(line)
		}

		newVal := c.newVar()
		c.use("setitem")
		c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", cur, idxRes[len(idxRes)-1].val, val.val, newVal))

		for i := len(containers) - 2; i >= 0; i-- {
			tmp := c.newVar()
			c.use("setitem")
			c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", containers[i], idxRes[i].val, newVal, tmp))
			newVal = tmp
		}
		c.writeln(fmt.Sprintf("%s = %s,", sanitizeVar(a.Name), newVal))
		return nil
	}
	if name, ok := c.vars[a.Name]; ok {
		cur := c.newVar()
		c.writeln(fmt.Sprintf("nb_getval(%s, %s),", name, cur))
		idxRes := make([]exprRes, len(a.Index))
		for i, idx := range a.Index {
			r, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			idxRes[i] = r
		}
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return err
		}
		containers := []string{cur}
		for _, r := range idxRes[:len(idxRes)-1] {
			for _, line := range r.code {
				c.writeln(line)
			}
			next := c.newVar()
			c.use("getitem")
			c.writeln(fmt.Sprintf("get_item(%s, %s, %s),", cur, r.val, next))
			cur = next
			containers = append(containers, cur)
		}
		for _, line := range idxRes[len(idxRes)-1].code {
			c.writeln(line)
		}
		for _, line := range val.code {
			c.writeln(line)
		}

		newVal := c.newVar()
		c.use("setitem")
		c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", cur, idxRes[len(idxRes)-1].val, val.val, newVal))

		for i := len(containers) - 2; i >= 0; i-- {
			tmp := c.newVar()
			c.use("setitem")
			c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", containers[i], idxRes[i].val, newVal, tmp))
			newVal = tmp
		}
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", name, newVal))
		return nil
	}
	if allSimple(a.Index) {
		idxRes := make([]exprRes, len(a.Index))
		for i, idx := range a.Index {
			r, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			idxRes[i] = r
		}
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return err
		}
		cur := sanitizeVar(a.Name)
		containers := []string{cur}
		for _, r := range idxRes[:len(idxRes)-1] {
			for _, line := range r.code {
				c.writeln(line)
			}
			next := c.newVar()
			c.use("getitem")
			c.writeln(fmt.Sprintf("get_item(%s, %s, %s),", cur, r.val, next))
			cur = next
			containers = append(containers, cur)
		}
		for _, line := range idxRes[len(idxRes)-1].code {
			c.writeln(line)
		}
		for _, line := range val.code {
			c.writeln(line)
		}

		newVal := c.newVar()
		c.use("setitem")
		c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", cur, idxRes[len(idxRes)-1].val, val.val, newVal))

		for i := len(containers) - 2; i >= 0; i-- {
			tmp := c.newVar()
			c.use("setitem")
			c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", containers[i], idxRes[i].val, newVal, tmp))
			newVal = tmp
		}
		c.writeln(fmt.Sprintf("%s = %s,", sanitizeVar(a.Name), newVal))
		return nil
	}
	return fmt.Errorf("unsupported assignment")
}

func allSimple(idxs []*parser.IndexOp) bool {
	for _, idx := range idxs {
		if idx.Colon != nil {
			return false
		}
	}
	return true
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt, ret string) error {
	c.writeln("catch(")
	c.indent++
	c.writeln("(")
	c.indent++
	c.writeln("repeat,")
	c.indent++
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	for _, line := range cond.code {
		c.writeln(line)
	}
	c.writeln(fmt.Sprintf("(%s ->", cond.val))
	c.indent++
	c.writeln("catch(")
	c.indent++
	c.writeln("(")
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s, ret); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("), continue, true),")
	c.writeln("fail")
	c.indent--
	c.writeln("; true)")
	c.indent--
	c.indent--
	c.writeln(")")
	c.writeln(", break, true),")
	c.indent--
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	name, ok := c.vars[u.Target]
	if !ok {
		return fmt.Errorf("update of immutable variable not supported")
	}
	cur := c.newVar()
	c.writeln(fmt.Sprintf("nb_getval(%s, %s),", name, cur))
	listVar := c.newVar()
	c.use("tolist")
	c.writeln(fmt.Sprintf("to_list(%s, %s),", cur, listVar))

	itemVar := c.newVar()
	newItem := c.newVar()
	resultVar := c.newVar()

	parts := []string{fmt.Sprintf("member(%s, %s)", itemVar, listVar)}
	if c.env != nil {
		if typ, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := typ.(types.ListType); ok {
				if st, ok := lt.Elem.(types.StructType); ok {
					for _, f := range st.Order {
						parts = append(parts, fmt.Sprintf("get_dict(%s, %s, %s)", sanitizeAtom(f), itemVar, sanitizeVar(f)))
					}
				}
			}
		}
	}

	var condStr string
	if u.Where != nil {
		condRes, err := c.compileExpr(u.Where)
		if err != nil {
			return err
		}
		for _, line := range condRes.code {
			parts = append(parts, strings.TrimSuffix(line, ","))
		}
		condStr = condRes.val
	}

	curItem := itemVar
	updateParts := []string{}
	for _, it := range u.Set.Items {
		valRes, err := c.compileExpr(it.Value)
		if err != nil {
			return err
		}
		for _, line := range valRes.code {
			updateParts = append(updateParts, strings.TrimSuffix(line, ","))
		}
		key, _ := identName(it.Key)
		tmp := c.newVar()
		c.use("setitem")
		updateParts = append(updateParts, fmt.Sprintf("set_item(%s, %s, %s, %s)", curItem, sanitizeAtom(key), valRes.val, tmp))
		curItem = tmp
	}
	updateParts = append(updateParts, fmt.Sprintf("%s = %s", newItem, curItem))
	updateBody := strings.Join(updateParts, ", ")

	if u.Where != nil {
		parts = append(parts, fmt.Sprintf("(%s -> %s ; %s = %s)", condStr, updateBody, newItem, itemVar))
	} else {
		parts = append(parts, updateBody)
	}

	goal := strings.Join(parts, ", ")
	c.writeln(fmt.Sprintf("findall(%s, (%s), %s),", newItem, goal, resultVar))
	c.writeln(fmt.Sprintf("nb_setval(%s, %s),", name, resultVar))
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	res, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	for _, line := range res.code {
		c.writeln(line)
	}
	c.use("expect")
	c.writeln(fmt.Sprintf("expect(%s),", res.val))
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeAtom(strings.ReplaceAll(t.Name, " ", "_"))
	c.tests = append(c.tests, name)
	body, err := c.compileBlock(t.Body, "_")
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s :-", name))
	c.indent++
	if len(body) > 0 {
		body = bytes.TrimSuffix(body, []byte("\n"))
		c.buf.Write(body)
		c.writeln(",")
	}
	c.writeln("true.")
	c.indent--
	return nil
}

func pureFunExpr(e *parser.Expr) *parser.FunExpr {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return nil
	}
	return p.Target.FunExpr
}

// compileTypeDecl currently emits no Prolog code as type information is
// only used during type checking. Supporting the statement ensures that
// type definitions inside functions do not cause compile errors.
func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	// Methods are ignored since the Prolog backend has no notion of
	// attaching functions to structs.
	return nil
}
