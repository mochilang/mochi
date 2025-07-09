//go:build slow

package rbcode

import (
	"bytes"
	"fmt"
	"math"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Ruby source code.
type Compiler struct {
	buf           bytes.Buffer
	indent        int
	env           *types.Env
	tmpCount      int
	useOpenStruct bool
	helpers       map[string]bool
	structs       map[string]bool
	globals       map[string]bool
	queryRows     map[string][]string
	inKeyContext  bool
}

// New creates a new Ruby compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, helpers: make(map[string]bool), structs: make(map[string]bool), globals: make(map[string]bool), queryRows: make(map[string][]string)}
}

func (c *Compiler) newTmp() string {
	name := fmt.Sprintf("_t%d", c.tmpCount)
	c.tmpCount++
	return name
}

// Compile generates Ruby code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	// reset state so helpers from previous compilations aren't leaked
	c.helpers = map[string]bool{}
	c.useOpenStruct = false
	// collect top level globals so functions defined before variables can
	// still reference them as globals
	for _, s := range prog.Statements {
		switch {
		case s.Let != nil:
			c.globals[sanitizeName(s.Let.Name)] = true
		case s.Var != nil:
			c.globals[sanitizeName(s.Var.Name)] = true
		}
	}
	tests := []*parser.TestBlock{}
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Test != nil {
			tests = append(tests, s.Test)
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, t := range tests {
		if err := c.compileTestBlock(t); err != nil {
			return nil, err
		}
	}
	body := append([]byte(nil), c.buf.Bytes()...)
	c.buf.Reset()
	if c.useOpenStruct {
		c.writeln("require 'ostruct'")
		c.writeln("")
	}
	if len(c.helpers) > 0 {
		c.emitRuntime()
		c.writeln("")
	}
	c.buf.Write(body)
	src := c.buf.Bytes()
	return FormatRB(src), nil
}

// --- Statements ---
func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("return " + val)
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Fun != nil:
		return c.compileLocalFunStmt(s.Fun)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("next")
		return nil
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	default:
		return nil
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	val := "nil"
	if l.Value != nil {
		v, err := c.compileExpr(l.Value)
		if err != nil {
			return err
		}
		val = v
	}
	if c.env != nil {
		var typ types.Type = types.AnyType{}
		if l.Type != nil {
			typ = c.resolveTypeRef(l.Type)
		} else if l.Value != nil {
			typ = c.inferExprType(l.Value)
		}
		c.env.SetVar(l.Name, typ, false)
	}
	name := sanitizeName(l.Name)
	if c.indent == 0 {
		c.globals[name] = true
		name = "$" + name
	}
	c.writeln(fmt.Sprintf("%s = %s", name, val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	val := "nil"
	if v.Value != nil {
		e, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		val = e
	}
	if c.env != nil {
		var typ types.Type = types.AnyType{}
		if v.Type != nil {
			typ = c.resolveTypeRef(v.Type)
		} else if v.Value != nil {
			typ = c.inferExprType(v.Value)
		} else if t, err := c.env.GetVar(v.Name); err == nil {
			typ = t
		}
		c.env.SetVar(v.Name, typ, true)
	}
	name := sanitizeName(v.Name)
	if c.indent == 0 {
		c.globals[name] = true
		name = "$" + name
	}
	c.writeln(fmt.Sprintf("%s = %s", name, val))
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("module %s; end", name))
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			if len(v.Fields) == 0 {
				c.writeln(fmt.Sprintf("class %s", vname))
				c.indent++
				c.writeln(fmt.Sprintf("include %s", name))
				c.indent--
				c.writeln("end")
				continue
			}
			fields := []string{}
			for _, f := range v.Fields {
				fields = append(fields, ":"+sanitizeName(f.Name))
			}
			fieldList := strings.Join(fields, ", ")
			if fieldList != "" {
				fieldList += ", "
			}
			c.writeln(fmt.Sprintf("%s = Struct.new(%skeyword_init: true) do", vname, fieldList))
			c.indent++
			c.writeln(fmt.Sprintf("include %s", name))
			c.indent--
			c.writeln("end")
			for _, f := range v.Fields {
				if st, ok := c.resolveTypeRef(f.Type).(types.StructType); ok {
					if err := c.compileStructType(st); err != nil {
						return err
					}
				}
			}
		}
		return nil
	}
	fields := []string{}
	var methods []*parser.FunStmt
	for _, m := range t.Members {
		if m.Field != nil {
			fields = append(fields, ":"+sanitizeName(m.Field.Name))
		} else if m.Method != nil {
			methods = append(methods, m.Method)
		}
	}
	fieldList := strings.Join(fields, ", ")
	if fieldList != "" {
		fieldList += ", "
	}
	if len(methods) == 0 {
		c.writeln(fmt.Sprintf("%s = Struct.new(%skeyword_init: true)", name, fieldList))
	} else {
		c.writeln(fmt.Sprintf("%s = Struct.new(%skeyword_init: true) do", name, fieldList))
		c.indent++
		for _, m := range methods {
			if err := c.compileMethod(m); err != nil {
				return err
			}
			c.writeln("")
		}
		c.indent--
		c.writeln("end")
	}
	for _, m := range t.Members {
		if m.Field != nil {
			if st, ok := c.resolveTypeRef(m.Field.Type).(types.StructType); ok {
				if err := c.compileStructType(st); err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func (c *Compiler) compileFunStmt(fn *parser.FunStmt) error {
	if c.env != nil {
		c.env.SetFunc(fn.Name, fn)
	}
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("def %s(%s)", sanitizeName(fn.Name), strings.Join(params, ", ")))
	var orig *types.Env
	if c.env != nil {
		orig = c.env
		child := types.NewEnv(c.env)
		for _, p := range fn.Params {
			child.SetVar(p.Name, types.AnyType{}, true)
		}
		c.env = child
	}
	c.indent++
	for _, s := range fn.Body {
		if err := c.compileStmt(s); err != nil {
			if orig != nil {
				c.env = orig
			}
			return err
		}
	}
	c.indent--
	if orig != nil {
		c.env = orig
	}
	c.writeln("end")
	return nil
}

func (c *Compiler) compileLocalFunStmt(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("%s = ->(%s){", sanitizeName(fn.Name), strings.Join(params, ", ")))
	var orig *types.Env
	if c.env != nil {
		orig = c.env
		child := types.NewEnv(c.env)
		for _, p := range fn.Params {
			child.SetVar(p.Name, types.AnyType{}, true)
		}
		c.env = child
	}
	c.indent++
	for _, s := range fn.Body {
		if err := c.compileStmt(s); err != nil {
			if orig != nil {
				c.env = orig
			}
			return err
		}
	}
	c.indent--
	if orig != nil {
		c.env = orig
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileMethod(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	c.writeln(fmt.Sprintf("def %s(%s)", sanitizeName(fn.Name), strings.Join(params, ", ")))
	c.indent++
	for _, s := range fn.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	first := true
	for stmt != nil {
		cond, err := c.compileExpr(stmt.Cond)
		if err != nil {
			return err
		}
		if first {
			c.writeln("if " + cond)
			first = false
		} else {
			c.writeln("elsif " + cond)
		}
		c.indent++
		for _, s := range stmt.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		if stmt.ElseIf != nil {
			stmt = stmt.ElseIf
			continue
		}
		if len(stmt.Else) > 0 {
			c.writeln("else")
			c.indent++
			for _, s := range stmt.Else {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
			c.indent--
		}
		break
	}
	c.writeln("end")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s...%s", name, start, end))
	} else {
		src, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		if isStringLiteral(stmt.Source) {
			c.writeln(fmt.Sprintf("for %s in %s.chars", name, src))
		} else if types.IsMapType(c.inferExprType(stmt.Source)) {
			c.writeln(fmt.Sprintf("for %s in %s.keys", name, src))
		} else {
			c.writeln(fmt.Sprintf("for %s in %s", name, src))
		}
	}
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond)
	c.indent++
	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("end")
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
	val, err := c.compileExpr(s.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, val))
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
		return fmt.Sprintf("->(%s){ %s }", strings.Join(params, ", "), expr), nil
	}
	if len(fn.BlockBody) > 0 {
		var childEnv *types.Env
		if c.env != nil {
			childEnv = types.NewEnv(c.env)
			for _, p := range fn.Params {
				childEnv.SetVar(p.Name, types.AnyType{}, true)
			}
		}
		sub := &Compiler{env: childEnv, indent: c.indent + 1, helpers: c.helpers, useOpenStruct: c.useOpenStruct, tmpCount: c.tmpCount}
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				return "", err
			}
		}
		body := sub.buf.String()
		c.tmpCount = sub.tmpCount
		if sub.useOpenStruct {
			c.useOpenStruct = true
		}
		var b strings.Builder
		b.WriteString("->(" + strings.Join(params, ", ") + "){\n")
		b.WriteString(body)
		for i := 0; i < c.indent; i++ {
			b.WriteByte('\t')
		}
		b.WriteString("}")
		return b.String(), nil
	}
	return "", fmt.Errorf("empty function body")
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	needsHelper := false
	var rowMapping []string
	for _, j := range q.Joins {
		if j.Side != nil {
			needsHelper = true
			break
		}
	}
	if q.Group != nil && (len(q.Froms) > 0 || len(q.Joins) > 0 || q.Where != nil || q.Sort != nil || q.Skip != nil || q.Take != nil) {
		needsHelper = true
	}

	if needsHelper {
		origEnv := c.env
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
		sides := make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				return "", err
			}
			joinSrcs[i] = js
			on, err := c.compileExpr(j.On)
			if err != nil {
				return "", err
			}
			joinOns[i] = on
			if j.Side != nil {
				sides[i] = *j.Side
			}
		}
		var sel string
		var keyExpr string
		if q.Group != nil {
			keyExpr, err = c.compileExpr(q.Group.Exprs[0])
			if err != nil {
				return "", err
			}
		} else {
			sel, err = c.compileExpr(q.Select)
			if err != nil {
				return "", err
			}
		}
		var whereStr, sortStr, skipStr, takeStr string
		if q.Where != nil {
			whereStr, err = c.compileExpr(q.Where)
			if err != nil {
				return "", err
			}
		}
		if q.Sort != nil {
			sortStr, err = c.compileExprKey(q.Sort)
			if err != nil {
				return "", err
			}
		}
		if q.Skip != nil {
			skipStr, err = c.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
		}
		if q.Take != nil {
			takeStr, err = c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
		}
		names := []string{sanitizeName(q.Var)}
		for _, f := range q.Froms {
			names = append(names, sanitizeName(f.Var))
		}
		params := append([]string(nil), names...)
		joins := make([]string, 0, len(q.Froms)+len(q.Joins))
		for _, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("{ 'items' => %s }", fs))
		}
		tmp := append([]string(nil), params...)
		for i, js := range joinSrcs {
			onParams := append(tmp, sanitizeName(q.Joins[i].Var))
			spec := fmt.Sprintf("{ 'items' => %s, 'on' => ->(%s){ %s }", js, strings.Join(onParams, ", "), joinOns[i])
			if sides[i] == "left" || sides[i] == "outer" {
				spec += ", 'left' => true"
			}
			if sides[i] == "right" || sides[i] == "outer" {
				spec += ", 'right' => true"
			}
			spec += " }"
			joins = append(joins, spec)
			tmp = append(tmp, sanitizeName(q.Joins[i].Var))
		}
		allParams := strings.Join(tmp, ", ")
		var selectFn string
		if q.Group != nil {
			tupleExpr := "[" + allParams + "]"
			selectFn = fmt.Sprintf("->(%s){ %s }", allParams, tupleExpr)
		} else {
			selectFn = fmt.Sprintf("->(%s){ %s }", allParams, sel)
		}
		var whereFn, sortFn string
		if whereStr != "" {
			whereFn = fmt.Sprintf("->(%s){ %s }", allParams, whereStr)
		}
		if sortStr != "" && q.Group == nil {
			sortFn = fmt.Sprintf("->(%s){ %s }", allParams, sortStr)
		}
		var sortExpr, skipExpr, takeExpr string
		if q.Group != nil {
			vars := append([]string{sanitizeName(q.Var)}, names[1:]...)
			for _, j := range q.Joins {
				vars = append(vars, sanitizeName(j.Var))
			}
			c.queryRows[q.Group.Name] = vars
			if q.Sort != nil {
				genv := types.NewEnv(c.env)
				genv.SetVar(q.Group.Name, types.AnyType{}, true)
				c.env = genv
				sortExpr, err = c.compileExprKey(q.Sort)
				c.env = origEnv
				if err != nil {
					delete(c.queryRows, q.Group.Name)
					return "", err
				}
			}
			if q.Skip != nil {
				genv := types.NewEnv(c.env)
				genv.SetVar(q.Group.Name, types.AnyType{}, true)
				c.env = genv
				skipExpr, err = c.compileExpr(q.Skip)
				c.env = origEnv
				if err != nil {
					delete(c.queryRows, q.Group.Name)
					return "", err
				}
			}
			if q.Take != nil {
				genv := types.NewEnv(c.env)
				genv.SetVar(q.Group.Name, types.AnyType{}, true)
				c.env = genv
				takeExpr, err = c.compileExpr(q.Take)
				c.env = origEnv
				if err != nil {
					delete(c.queryRows, q.Group.Name)
					return "", err
				}
			}
			delete(c.queryRows, q.Group.Name)
		}
		var b strings.Builder
		b.WriteString("(begin\n")
		b.WriteString("\tsrc = " + src + "\n")
		b.WriteString("\t_rows = _query(src, [\n")
		for i, j := range joins {
			b.WriteString("\t\t" + j)
			if i != len(joins)-1 {
				b.WriteString(",")
			}
			b.WriteString("\n")
		}
		b.WriteString("\t], { 'select' => " + selectFn)
		if whereFn != "" {
			b.WriteString(", 'where' => " + whereFn)
		}
		if sortFn != "" {
			b.WriteString(", 'sortKey' => " + sortFn)
		}
		if skipStr != "" && q.Group == nil {
			b.WriteString(", 'skip' => " + skipStr)
		}
		if takeStr != "" && q.Group == nil {
			b.WriteString(", 'take' => " + takeStr)
		}
		b.WriteString(" })\n")
		if q.Group != nil {
			b.WriteString("\t_groups = _group_by(_rows, ->(" + allParams + "){ " + keyExpr + " })\n")
			tmpName := fmt.Sprintf("_items%d", c.tmpCount)
			c.tmpCount++
			b.WriteString("\t" + tmpName + " = _groups\n")
			if sortExpr != "" {
				b.WriteString("\t" + tmpName + " = " + tmpName + ".sort_by { |" + sanitizeName(q.Group.Name) + "| " + sortExpr + " }\n")
			}
			if skipExpr != "" {
				b.WriteString(fmt.Sprintf("\t%s = (%s).drop(%s)\n", tmpName, tmpName, skipExpr))
			}
			if takeExpr != "" {
				b.WriteString(fmt.Sprintf("\t%s = (%s).take(%s)\n", tmpName, tmpName, takeExpr))
			}
			b.WriteString("\t_res = []\n")
			b.WriteString("\tfor " + sanitizeName(q.Group.Name) + " in " + tmpName + "\n")
			var valExpr string
			vars := append([]string{sanitizeName(q.Var)}, names[1:]...)
			for _, j := range q.Joins {
				vars = append(vars, sanitizeName(j.Var))
			}
			c.queryRows[q.Group.Name] = vars
			if c.env != nil {
				origEnv := c.env
				genv := types.NewEnv(c.env)
				genv.SetVar(q.Group.Name, types.AnyType{}, true)
				c.env = genv
				valExpr, err = c.compileExpr(q.Select)
				c.env = origEnv
				if err != nil {
					delete(c.queryRows, q.Group.Name)
					return "", err
				}
			} else {
				valExpr, err = c.compileExpr(q.Select)
				if err != nil {
					delete(c.queryRows, q.Group.Name)
					return "", err
				}
			}
			delete(c.queryRows, q.Group.Name)
			b.WriteString("\t\t_res << " + valExpr + "\n")
			b.WriteString("\tend\n")
			b.WriteString("\t_res\n")
			b.WriteString("end)")
			c.use("_group_by")
			c.use("_group")
			c.use("_query")
			return b.String(), nil
		}
		b.WriteString("\t_rows\n")
		b.WriteString("end)")
		c.use("_query")
		return b.String(), nil
	}

	if len(q.Joins) > 0 && !needsHelper {
		src, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		iter := sanitizeName(q.Var)
		if name, ok := identName(q.Source); ok {
			if vars, ok2 := c.queryRows[name]; ok2 {
				rowMapping = vars
				c.queryRows[q.Var] = vars
			}
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
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				return "", err
			}
			joinSrcs[i] = js
			on, err := c.compileExpr(j.On)
			if err != nil {
				return "", err
			}
			joinOns[i] = on
			if j.Side != nil {
				joinSides[i] = *j.Side
			}
		}
		specialLeft := len(q.Joins) == 1 && joinSides[0] == "left"
		specialRight := len(q.Joins) == 1 && joinSides[0] == "right"
		specialOuter := len(q.Joins) == 1 && joinSides[0] == "outer"
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		var sortVal, skipVal, takeVal string
		if q.Sort != nil {
			v, err := c.compileExprKey(q.Sort)
			if err != nil {
				if rowMapping != nil {
					delete(c.queryRows, q.Var)
				}
				return "", err
			}
			sortVal = v
		}
		if q.Skip != nil {
			v, err := c.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
			skipVal = v
		}
		if q.Take != nil {
			v, err := c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
			takeVal = v
		}
		var condStr string
		if q.Where != nil {
			var err error
			condStr, err = c.compileExpr(q.Where)
			if err != nil {
				return "", err
			}
		}
		var b strings.Builder
		b.WriteString("(begin\n")
		b.WriteString("\t_res = []\n")
		b.WriteString(fmt.Sprintf("\tfor %s in %s\n", iter, src))
		indent := "\t\t"
		for i, f := range q.Froms {
			b.WriteString(fmt.Sprintf("%sfor %s in %s\n", indent, sanitizeName(f.Var), fromSrcs[i]))
			indent += "\t"
		}
		if specialLeft {
			b.WriteString(fmt.Sprintf("%smatched = false\n", indent))
			j := q.Joins[0]
			b.WriteString(fmt.Sprintf("%sfor %s in %s\n", indent, sanitizeName(j.Var), joinSrcs[0]))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%sif %s\n", indent, joinOns[0]))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%smatched = true\n", indent))
		} else if specialRight {
			b.WriteString(fmt.Sprintf("%smatched = false\n", indent))
			j := q.Joins[0]
			b.WriteString(fmt.Sprintf("%sfor %s in %s\n", indent, sanitizeName(j.Var), joinSrcs[0]))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%sfor %s in %s\n", indent, iter, src))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%sif %s\n", indent, joinOns[0]))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%smatched = true\n", indent))
		} else if specialOuter {
			b.WriteString(fmt.Sprintf("%smatched = false\n", indent))
			j := q.Joins[0]
			b.WriteString(fmt.Sprintf("%sfor %s in %s\n", indent, sanitizeName(j.Var), joinSrcs[0]))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%sif %s\n", indent, joinOns[0]))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%smatched = true\n", indent))
		} else {
			for i, j := range q.Joins {
				b.WriteString(fmt.Sprintf("%sfor %s in %s\n", indent, sanitizeName(j.Var), joinSrcs[i]))
				indent += "\t"
				b.WriteString(fmt.Sprintf("%sif %s\n", indent, joinOns[i]))
				indent += "\t"
			}
		}
		if q.Where != nil {
			b.WriteString(fmt.Sprintf("%sif %s\n", indent, condStr))
			indent += "\t"
			if sortVal != "" {
				b.WriteString(fmt.Sprintf("%s_res << [%s, %s]\n", indent, sortVal, sel))
			} else {
				b.WriteString(fmt.Sprintf("%s_res << %s\n", indent, sel))
			}
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		} else {
			if sortVal != "" {
				b.WriteString(fmt.Sprintf("%s_res << [%s, %s]\n", indent, sortVal, sel))
			} else {
				b.WriteString(fmt.Sprintf("%s_res << %s\n", indent, sel))
			}
		}
		if specialLeft {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			b.WriteString(fmt.Sprintf("%sif !matched\n", indent))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%s%s = nil\n", indent, sanitizeName(q.Joins[0].Var)))
			if q.Where != nil {
				b.WriteString(fmt.Sprintf("%sif %s\n", indent, condStr))
				indent += "\t"
			}
			if sortVal != "" {
				b.WriteString(fmt.Sprintf("%s_res << [%s, %s]\n", indent, sortVal, sel))
			} else {
				b.WriteString(fmt.Sprintf("%s_res << %s\n", indent, sel))
			}
			if q.Where != nil {
				indent = indent[:len(indent)-1]
				b.WriteString(indent + "end\n")
			}
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		} else if specialRight {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			b.WriteString(fmt.Sprintf("%sif !matched\n", indent))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%s%s = nil\n", indent, iter))
			if q.Where != nil {
				b.WriteString(fmt.Sprintf("%sif %s\n", indent, condStr))
				indent += "\t"
			}
			if sortVal != "" {
				b.WriteString(fmt.Sprintf("%s_res << [%s, %s]\n", indent, sortVal, sel))
			} else {
				b.WriteString(fmt.Sprintf("%s_res << %s\n", indent, sel))
			}
			if q.Where != nil {
				indent = indent[:len(indent)-1]
				b.WriteString(indent + "end\n")
			}
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		} else if specialOuter {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			b.WriteString(fmt.Sprintf("%sif !matched\n", indent))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%s%s = nil\n", indent, sanitizeName(q.Joins[0].Var)))
			if q.Where != nil {
				b.WriteString(fmt.Sprintf("%sif %s\n", indent, condStr))
				indent += "\t"
			}
			if sortVal != "" {
				b.WriteString(fmt.Sprintf("%s_res << [%s, %s]\n", indent, sortVal, sel))
			} else {
				b.WriteString(fmt.Sprintf("%s_res << %s\n", indent, sel))
			}
			if q.Where != nil {
				indent = indent[:len(indent)-1]
				b.WriteString(indent + "end\n")
			}
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			// unmatched right side
			b.WriteString(fmt.Sprintf("%sfor %s in %s\n", indent, sanitizeName(q.Joins[0].Var), joinSrcs[0]))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%smatched = false\n", indent))
			b.WriteString(fmt.Sprintf("%sfor %s in %s\n", indent, iter, src))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%sif %s\n", indent, joinOns[0]))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%smatched = true\n", indent))
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
			b.WriteString(fmt.Sprintf("%sif !matched\n", indent))
			indent += "\t"
			b.WriteString(fmt.Sprintf("%s%s = nil\n", indent, iter))
			if q.Where != nil {
				b.WriteString(fmt.Sprintf("%sif %s\n", indent, condStr))
				indent += "\t"
			}
			if sortVal != "" {
				b.WriteString(fmt.Sprintf("%s_res << [%s, %s]\n", indent, sortVal, sel))
			} else {
				b.WriteString(fmt.Sprintf("%s_res << %s\n", indent, sel))
			}
			if q.Where != nil {
				indent = indent[:len(indent)-1]
				b.WriteString(indent + "end\n")
			}
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		} else {
			for range q.Joins {
				indent = indent[:len(indent)-1]
				b.WriteString(indent + "end\n")
				indent = indent[:len(indent)-1]
				b.WriteString(indent + "end\n")
			}
		}
		for range q.Froms {
			indent = indent[:len(indent)-1]
			b.WriteString(indent + "end\n")
		}
		b.WriteString("\tend\n")
		if sortVal != "" {
			b.WriteString(fmt.Sprintf("\t_res = _res.sort_by { |e| e[0] }\n"))
			b.WriteString("\t_res = _res.map { |e| e[1] }\n")
		}
		if skipVal != "" {
			b.WriteString(fmt.Sprintf("\t_res = _res.drop(%s)\n", skipVal))
		}
		if takeVal != "" {
			b.WriteString(fmt.Sprintf("\t_res = _res.take(%s)\n", takeVal))
		}
		b.WriteString("\t_res\n")
		b.WriteString("end)")
		return b.String(), nil
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	iter := sanitizeName(q.Var)
	if name, ok := identName(q.Source); ok {
		if vars, ok2 := c.queryRows[name]; ok2 {
			rowMapping = vars
			c.queryRows[q.Var] = vars
		}
	}

	// simple grouping without additional clauses
	if q.Group != nil && len(q.Froms) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		c.use("_group_by")
		c.use("_group")
		return fmt.Sprintf("_group_by(%s, ->(%s){ %s }).map { |%s| %s }", src, iter, keyExpr, sanitizeName(q.Group.Name), valExpr), nil
	}

	// handle simple case without cross joins
	if len(q.Froms) == 0 {
		expr := fmt.Sprintf("(%s)", src)
		if q.Where != nil {
			cond, err := c.compileExpr(q.Where)
			if err != nil {
				if rowMapping != nil {
					delete(c.queryRows, q.Var)
				}
				return "", err
			}
			expr = fmt.Sprintf("(%s).select { |%s| %s }", expr, iter, cond)
		}
		if q.Sort != nil {
			val, err := c.compileExprKey(q.Sort)
			if err != nil {
				if rowMapping != nil {
					delete(c.queryRows, q.Var)
				}
				return "", err
			}
			expr = fmt.Sprintf("(%s).sort_by { |%s| %s }", expr, iter, val)
		}
		if q.Skip != nil {
			val, err := c.compileExpr(q.Skip)
			if err != nil {
				if rowMapping != nil {
					delete(c.queryRows, q.Var)
				}
				return "", err
			}
			expr = fmt.Sprintf("(%s).drop(%s)", expr, val)
		}
		if q.Take != nil {
			val, err := c.compileExpr(q.Take)
			if err != nil {
				if rowMapping != nil {
					delete(c.queryRows, q.Var)
				}
				return "", err
			}
			expr = fmt.Sprintf("(%s).take(%s)", expr, val)
		}
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			if rowMapping != nil {
				delete(c.queryRows, q.Var)
			}
			return "", err
		}
		if rowMapping != nil {
			delete(c.queryRows, q.Var)
		}
		expr = fmt.Sprintf("(%s).map { |%s| %s }", expr, iter, sel)
		return expr, nil
	}

	var sortVal string
	if q.Sort != nil {
		v, err := c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		sortVal = v
	}

	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	var skipVal, takeVal string
	if q.Skip != nil {
		v, err := c.compileExpr(q.Skip)
		if err != nil {
			if rowMapping != nil {
				delete(c.queryRows, q.Var)
			}
			return "", err
		}
		skipVal = v
	}
	if q.Take != nil {
		v, err := c.compileExpr(q.Take)
		if err != nil {
			if rowMapping != nil {
				delete(c.queryRows, q.Var)
			}
			return "", err
		}
		takeVal = v
	}
	var b strings.Builder
	b.WriteString("(begin\n")
	b.WriteString("\t_res = []\n")
	b.WriteString(fmt.Sprintf("\tfor %s in %s\n", iter, src))
	indent := "\t\t"
	for i, f := range q.Froms {
		b.WriteString(fmt.Sprintf("%sfor %s in %s\n", indent, sanitizeName(f.Var), fromSrcs[i]))
		indent += "\t"
	}
	if q.Where != nil {
		cond, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf("%sif %s\n", indent, cond))
		indent += "\t"
		if sortVal != "" {
			b.WriteString(fmt.Sprintf("%s_res << [%s, %s]\n", indent, sortVal, sel))
		} else {
			b.WriteString(fmt.Sprintf("%s_res << %s\n", indent, sel))
		}
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "end\n")
	} else {
		if sortVal != "" {
			b.WriteString(fmt.Sprintf("%s_res << [%s, %s]\n", indent, sortVal, sel))
		} else {
			b.WriteString(fmt.Sprintf("%s_res << %s\n", indent, sel))
		}
	}
	for range q.Froms {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "end\n")
	}
	b.WriteString("\tend\n")
	if sortVal != "" {
		b.WriteString(fmt.Sprintf("\t_res = _res.sort_by { |e| e[0] }\n"))
		b.WriteString("\t_res = _res.map { |e| e[1] }\n")
	}
	if skipVal != "" {
		b.WriteString(fmt.Sprintf("\t_res = _res.drop(%s)\n", skipVal))
	}
	if takeVal != "" {
		b.WriteString(fmt.Sprintf("\t_res = _res.take(%s)\n", takeVal))
	}
	b.WriteString("\t_res\n")
	b.WriteString("end)")
	return b.String(), nil
}

// --- Expressions ---
func (c *Compiler) compileExpr(e *parser.Expr) (string, error) { return c.compileBinaryExpr(e.Binary) }

func (c *Compiler) compileExprKey(e *parser.Expr) (string, error) {
	saved := c.inKeyContext
	c.inKeyContext = true
	v, err := c.compileExpr(e)
	c.inKeyContext = saved
	return v, err
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", fmt.Errorf("nil binary expr")
	}

	operands := []string{}
	opTypes := []types.Type{}
	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, first)
	opTypes = append(opTypes, c.inferPostfixType(b.Left))

	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		opTypes = append(opTypes, c.inferPostfixType(&parser.Unary{Value: part.Right}))
		op := part.Op
		if part.All {
			op = op + "_all"
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
			rt := opTypes[i+1]

			var expr string
			switch op {
			case "in":
				if types.IsMapType(rt) {
					expr = fmt.Sprintf("(%s.to_h.key?(%s))", r, l)
				} else {
					expr = fmt.Sprintf("(%s.include?(%s))", r, l)
				}
			case "union":
				expr = fmt.Sprintf("(%s | %s)", l, r)
			case "union_all":
				expr = fmt.Sprintf("(%s + %s)", l, r)
			case "except":
				expr = fmt.Sprintf("(%s - %s)", l, r)
			case "intersect":
				expr = fmt.Sprintf("(%s & %s)", l, r)
			default:
				expr = fmt.Sprintf("(%s %s %s)", l, op, r)
			}

			operands[i] = expr
			opTypes[i] = types.AnyType{}
			operands = append(operands[:i+1], operands[i+2:]...)
			opTypes = append(opTypes[:i+1], opTypes[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state after binary compilation")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = fmt.Sprintf("(%s%s)", u.Ops[i], val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	isStr := c.isStringPostfix(p)
	for _, op := range p.Ops {
		if op.Index != nil {
			idx := op.Index
			if idx.Colon == nil {
				key, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				if isStr {
					c.use("_indexString")
					expr = fmt.Sprintf("_indexString(%s, %s)", expr, key)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, key)
				}
				isStr = false
			} else {
				start := "0"
				if idx.Start != nil {
					s, err := c.compileExpr(idx.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				endExpr := ""
				if idx.End != nil {
					e, err := c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
					endExpr = e
				} else {
					if isStr {
						endExpr = fmt.Sprintf("%s.length", expr)
					} else {
						endExpr = "-1"
					}
				}
				if isStr {
					c.use("_sliceString")
					expr = fmt.Sprintf("_sliceString(%s, %s, %s)", expr, start, endExpr)
				} else if idx.End != nil {
					expr = fmt.Sprintf("%s[%s...%s]", expr, start, endExpr)
				} else {
					expr = fmt.Sprintf("%s[%s..-1]", expr, start)
				}
				isStr = false
			}
		} else if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			if strings.HasSuffix(expr, ".contains") && len(args) == 1 {
				expr = strings.TrimSuffix(expr, ".contains")
				expr = fmt.Sprintf("(%s.include?(%s))", expr, args[0])
			} else if builtin, ok, err := c.compileBuiltinCall(expr, args, op.Call.Args); ok {
				if err != nil {
					return "", err
				}
				expr = builtin
			} else {
				argStr := strings.Join(args, ", ")
				if _, ok := c.env.GetFunc(expr); ok {
					expr = fmt.Sprintf("%s(%s)", expr, argStr)
				} else {
					expr = fmt.Sprintf("%s.call(%s)", expr, argStr)
				}
			}
		} else if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil && c.env != nil {
				if _, ok := c.env.GetStruct(*op.Cast.Type.Simple); ok {
					expr = fmt.Sprintf("%s.new(**(%s.to_h.transform_keys(&:to_sym)))", sanitizeName(*op.Cast.Type.Simple), expr)
					continue
				}
			}
			// Ruby is dynamically typed so other casts are no-ops.
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), v)
		}
		return fmt.Sprintf("%s.new(%s)", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.Query != nil:
		q, err := c.compileQueryExpr(p.Query)
		if err != nil {
			return "", err
		}
		return q, nil
	case p.If != nil:
		v, err := c.compileIfExpr(p.If)
		if err != nil {
			return "", err
		}
		return v, nil
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
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
		if len(p.Map.Items) == 0 {
			return "{}", nil
		}
		items := make([]string, len(p.Map.Items))
		vals := make([]string, len(p.Map.Items))
		identOnly := true
		for i, it := range p.Map.Items {
			var k string
			if id, ok := identName(it.Key); ok {
				k = sanitizeName(id)
			} else {
				identOnly = false
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
			vals[i] = v
			if identOnly {
				items[i] = fmt.Sprintf("%s: %s", k, v)
			} else {
				if id, ok := identName(it.Key); ok {
					k = strconv.Quote(sanitizeName(id))
				}
				items[i] = fmt.Sprintf("%s => %s", k, v)
			}
		}
		if identOnly {
			if c.inKeyContext {
				return "[" + strings.Join(vals, ", ") + "]", nil
			}
			c.useOpenStruct = true
			return "OpenStruct.new(" + strings.Join(items, ", ") + ")", nil
		}
		return "{" + strings.Join(items, ", ") + "}", nil
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if c.globals[name] {
			name = "$" + name
		}
		if vars, ok := c.queryRows[p.Selector.Root]; ok && len(p.Selector.Tail) > 0 {
			idx := -1
			for i, v := range vars {
				if sanitizeName(p.Selector.Tail[0]) == sanitizeName(v) {
					idx = i
					break
				}
			}
			if idx >= 0 {
				expr := fmt.Sprintf("%s[%d]", name, idx)
				for _, t := range p.Selector.Tail[1:] {
					expr += "." + sanitizeName(t)
				}
				return expr, nil
			}
			first := sanitizeName(p.Selector.Tail[0])
			if first == "items" {
				expr := fmt.Sprintf("%s.Items", name)
				for _, t := range p.Selector.Tail[1:] {
					expr += "." + sanitizeName(t)
				}
				return expr, nil
			}
			if first == "key" {
				expr := fmt.Sprintf("%s.key", name)
				for _, t := range p.Selector.Tail[1:] {
					expr += "." + sanitizeName(t)
				}
				return expr, nil
			}
			if first == "length" {
				expr := fmt.Sprintf("%s.length", name)
				for _, t := range p.Selector.Tail[1:] {
					expr += "." + sanitizeName(t)
				}
				return expr, nil
			}
		}
		var typ types.Type
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				typ = t
			}
		}
		if _, ok := typ.(types.MapType); ok {
			for i, t := range p.Selector.Tail {
				if i == 0 {
					name += fmt.Sprintf("[%q]", t)
				} else {
					name += fmt.Sprintf("[%q]", t)
				}
			}
			return name, nil
		}
		for _, t := range p.Selector.Tail {
			name += "." + sanitizeName(t)
		}
		if len(p.Selector.Tail) == 0 {
			if c.env != nil {
				if vt, err := c.env.GetVar(p.Selector.Root); err == nil {
					if ft, ok := vt.(types.FuncType); ok {
						if len(ft.Params) == 0 {
							return fmt.Sprintf("%s.new", name), nil
						}
						return fmt.Sprintf("method(:%s)", name), nil
					}
					return name, nil
				}
				if _, ok := c.env.GetFunc(p.Selector.Root); ok {
					return fmt.Sprintf("method(:%s)", name), nil
				}
			} else {
				return fmt.Sprintf("method(:%s)", name), nil
			}
		}
		return name, nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		name := sanitizeName(p.Call.Func)
		if builtin, ok, err := c.compileBuiltinCall(name, args, p.Call.Args); ok {
			if err != nil {
				return "", err
			}
			return builtin, nil
		}
		argStr := strings.Join(args, ", ")
		if c.env != nil {
			if vt, err := c.env.GetVar(p.Call.Func); err == nil {
				if ft, ok := vt.(types.FuncType); ok {
					if len(p.Call.Args) < len(ft.Params) {
						missing := len(ft.Params) - len(p.Call.Args)
						names := make([]string, missing)
						for i := range names {
							names[i] = c.newTmp()
						}
						callArgs := append(append([]string(nil), args...), names...)
						return fmt.Sprintf("->(%s){ %s(%s) }", strings.Join(names, ", "), name, strings.Join(callArgs, ", ")), nil
					}
				}
			}
		}
		if c.env != nil {
			if _, ok := c.env.GetFunc(p.Call.Func); ok {
				return fmt.Sprintf("%s(%s)", name, argStr), nil
			}
			if _, err := c.env.GetVar(p.Call.Func); err == nil {
				return fmt.Sprintf("%s.call(%s)", name, argStr), nil
			}
		}
		return fmt.Sprintf("%s.call(%s)", name, argStr), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Group != nil:
		v, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + v + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int), nil
	case l.Float != nil:
		if *l.Float == math.Trunc(*l.Float) {
			return strconv.FormatFloat(*l.Float, 'f', 1, 64), nil
		}
		return strconv.FormatFloat(*l.Float, 'f', -1, 64), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	case l.Null:
		return "nil", nil
	default:
		return "", fmt.Errorf("unknown literal")
	}
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	tmp := fmt.Sprintf("_t%d", c.tmpCount)
	c.tmpCount++
	var b strings.Builder
	b.WriteString("(begin\n")
	b.WriteString(fmt.Sprintf("\t%s = %s\n", tmp, target))
	b.WriteString("\tcase\n")
	for _, cs := range m.Cases {
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			b.WriteString("\telse\n")
			b.WriteString("\t\t" + res + "\n")
			b.WriteString("\tend\nend)")
			return b.String(), nil
		}
		cond := ""
		if call, ok := callPattern(cs.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				cond = fmt.Sprintf("%s.is_a?(%s)", tmp, sanitizeName(call.Func))
				names := []string{}
				values := []string{}
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						names = append(names, sanitizeName(id))
						field := sanitizeName(st.Order[idx])
						values = append(values, fmt.Sprintf("%s.%s", tmp, field))
					}
				}
				if len(names) > 0 {
					res = fmt.Sprintf("(->(%s){ %s }).call(%s)", strings.Join(names, ", "), res, strings.Join(values, ", "))
				}
			}
		} else if ident, ok := identName(cs.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond = fmt.Sprintf("%s.is_a?(%s)", tmp, sanitizeName(ident))
			}
		}
		if cond == "" {
			pat, err := c.compileExpr(cs.Pattern)
			if err != nil {
				return "", err
			}
			cond = fmt.Sprintf("%s == %s", tmp, pat)
		}
		b.WriteString("\twhen " + cond + "\n")
		b.WriteString("\t\t" + res + "\n")
	}
	b.WriteString("\telse\n\t\tnil\n\tend\nend)")
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
		elseExpr = "nil"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
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
		withStr = fmt.Sprintf("(%s).to_h.transform_keys(&:to_s)", w)
	} else {
		withStr = "nil"
	}
	c.use("_fetch")
	c.use("_structify")
	c.useOpenStruct = true
	return fmt.Sprintf("_structify(_fetch(%s, %s))", urlStr, withStr), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "nil"
	if l.Path != nil {
		rel := strings.TrimPrefix(*l.Path, "../")
		path = fmt.Sprintf("File.expand_path(%q, __dir__)", "../../../"+rel)
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("(%s).to_h.transform_keys(&:to_s)", v)
	}
	c.use("_load")
	expr := fmt.Sprintf("_load(%s, %s)", path, opts)
	if l.Type != nil && l.Type.Simple != nil {
		tname := sanitizeName(*l.Type.Simple)
		expr = fmt.Sprintf("(%s).map { |_it| %s.new(**_it) }", expr, tname)
	}
	return expr, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "nil"
	if s.Path != nil {
		path = strconv.Quote(*s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("(%s).to_h.transform_keys(&:to_s)", v)
	}
	c.use("_save")
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileBuiltinCall(name string, args []string, origArgs []*parser.Expr) (string, bool, error) {
	switch name {
	case "print":
		if len(args) == 1 && len(origArgs) == 1 {
			if c.isListExpr(origArgs[0]) || c.isMapExpr(origArgs[0]) {
				return fmt.Sprintf("puts(%s.inspect)", args[0]), true, nil
			}
		}
		return fmt.Sprintf("puts([%s].join(\" \"))", strings.Join(args, ", ")), true, nil
	case "len", "count":
		if len(args) != 1 {
			return "", true, fmt.Errorf("%s expects 1 arg", name)
		}
		return fmt.Sprintf("(%s).length", args[0]), true, nil
	case "concat":
		if len(args) == 0 {
			return "[]", true, nil
		}
		expr := args[0]
		for _, a := range args[1:] {
			expr = fmt.Sprintf("(%s + %s)", expr, a)
		}
		return expr, true, nil
	case "append":
		if len(args) != 2 {
			return "", true, fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("(%s + [%s])", args[0], args[1]), true, nil
	case "substr", "substring":
		if len(args) != 3 {
			return "", true, fmt.Errorf("substr expects 3 args")
		}
		c.use("_sliceString")
		return fmt.Sprintf("_sliceString(%s, %s, %s)", args[0], args[1], args[2]), true, nil
	case "reverse":
		if len(args) != 1 {
			return "", true, fmt.Errorf("reverse expects 1 arg")
		}
		c.use("_reverse")
		return fmt.Sprintf("_reverse(%s)", args[0]), true, nil
	case "split":
		if len(args) != 2 {
			return "", true, fmt.Errorf("split expects 2 args")
		}
		c.use("_splitString")
		return fmt.Sprintf("_splitString(%s, %s)", args[0], args[1]), true, nil
	case "join":
		if len(args) != 2 {
			return "", true, fmt.Errorf("join expects 2 args")
		}
		c.use("_joinStrings")
		return fmt.Sprintf("_joinStrings(%s, %s)", args[0], args[1]), true, nil
	case "values":
		if len(args) != 1 {
			return "", true, fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("(%s).values", args[0]), true, nil
	case "str":
		if len(args) != 1 {
			return "", true, fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("(%s).to_s", args[0]), true, nil
	case "upper":
		if len(args) != 1 {
			return "", true, fmt.Errorf("upper expects 1 arg")
		}
		return fmt.Sprintf("(%s).to_s.upcase", args[0]), true, nil
	case "lower":
		if len(args) != 1 {
			return "", true, fmt.Errorf("lower expects 1 arg")
		}
		return fmt.Sprintf("(%s).to_s.downcase", args[0]), true, nil
	case "avg":
		if len(args) != 1 {
			return "", true, fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("((%[1]s).length > 0 ? (%[1]s).sum(0.0) / (%[1]s).length : 0)", args[0]), true, nil
	case "sum":
		if len(args) != 1 {
			return "", true, fmt.Errorf("sum expects 1 arg")
		}
		c.use("_sum")
		return fmt.Sprintf("_sum(%s)", args[0]), true, nil
	case "exists":
		if len(args) != 1 {
			return "", true, fmt.Errorf("exists expects 1 arg")
		}
		return fmt.Sprintf("!(%s).empty?", args[0]), true, nil
	case "min":
		if len(args) != 1 {
			return "", true, fmt.Errorf("min expects 1 arg")
		}
		c.use("_min")
		return fmt.Sprintf("_min(%s)", args[0]), true, nil
	case "max":
		if len(args) != 1 {
			return "", true, fmt.Errorf("max expects 1 arg")
		}
		c.use("_max")
		return fmt.Sprintf("_max(%s)", args[0]), true, nil
	case "first":
		if len(args) != 1 {
			return "", true, fmt.Errorf("first expects 1 arg")
		}
		c.use("_first")
		return fmt.Sprintf("_first(%s)", args[0]), true, nil
	case "json":
		if len(args) != 1 {
			return "", true, fmt.Errorf("json expects 1 arg")
		}
		c.use("_json")
		return fmt.Sprintf("_json(%s)", args[0]), true, nil
	case "input":
		if len(args) != 0 {
			return "", true, fmt.Errorf("input expects no args")
		}
		return "STDIN.gets.to_s.strip", true, nil
	case "eval":
		if len(args) != 1 {
			return "", true, fmt.Errorf("eval expects 1 arg")
		}
		c.use("_eval")
		return fmt.Sprintf("_eval(%s)", args[0]), true, nil
	default:
		return "", false, nil
	}
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
			params = append(params, fmt.Sprintf("%q => %s", f.Name, v))
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
		paramStr = "{" + strings.Join(params, ", ") + "}"
	}
	if model == "" {
		model = "\"\""
	}
	switch g.Target {
	case "embedding":
		c.use("_genEmbed")
		return fmt.Sprintf("_gen_embed(%s, %s, %s)", text, model, paramStr), nil
	default:
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_genStruct")
			return fmt.Sprintf("_gen_struct(%s, %s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
		}
		c.use("_genText")
		return fmt.Sprintf("_gen_text(%s, %s, %s)", prompt, model, paramStr), nil
	}
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileStructType(st types.StructType) error {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	fields := make([]string, len(st.Order))
	for i, fn := range st.Order {
		fields[i] = ":" + sanitizeName(fn)
	}
	fieldList := strings.Join(fields, ", ")
	if fieldList != "" {
		fieldList += ", "
	}
	if len(st.Methods) == 0 {
		c.writeln(fmt.Sprintf("%s = Struct.new(%skeyword_init: true)", name, fieldList))
	} else {
		c.writeln(fmt.Sprintf("%s = Struct.new(%skeyword_init: true) do", name, fieldList))
		c.indent++
		base := types.NewEnv(c.env)
		for _, fn := range st.Order {
			base.SetVar(fn, st.Fields[fn], true)
		}
		orig := c.env
		c.env = base
		for _, m := range st.Methods {
			if err := c.compileMethod(m.Decl); err != nil {
				c.env = orig
				return err
			}
			c.writeln("")
		}
		c.env = orig
		c.indent--
		c.writeln("end")
	}
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			if err := c.compileStructType(sub); err != nil {
				return err
			}
		}
	}
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("raise \"expect failed\" unless %s", expr))
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	list := sanitizeName(u.Target)
	idxVar := c.newTmp()
	itemVar := c.newTmp()
	c.writeln(fmt.Sprintf("%s.each_with_index do |%s, %s|", list, itemVar, idxVar))
	c.indent++

	var elem types.Type = types.AnyType{}
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				elem = lt.Elem
			}
		}
	}

	used := map[string]struct{}{}
	if u.Where != nil {
		collectIdents(u.Where, used)
	}
	for _, it := range u.Set.Items {
		if key, ok := identName(it.Key); ok {
			used[key] = struct{}{}
		}
		collectIdents(it.Value, used)
	}

	var orig *types.Env
	if st, ok := elem.(types.StructType); ok {
		child := types.NewEnv(c.env)
		for _, f := range st.Order {
			if _, ok := used[f]; !ok {
				continue
			}
			c.writeln(fmt.Sprintf("%s = %s.%s", sanitizeName(f), itemVar, sanitizeName(f)))
			child.SetVar(f, st.Fields[f], true)
		}
		orig = c.env
		c.env = child
	} else if mt, ok := elem.(types.MapType); ok && equalTypes(mt.Key, types.StringType{}) {
		child := types.NewEnv(c.env)
		for f := range used {
			c.writeln(fmt.Sprintf("%s = %s[%q]", sanitizeName(f), itemVar, f))
			child.SetVar(f, mt.Value, true)
		}
		orig = c.env
		c.env = child
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			if orig != nil {
				c.env = orig
			}
			return err
		}
		c.writeln(fmt.Sprintf("next unless %s", cond))
	}

	for _, it := range u.Set.Items {
		if _, ok := elem.(types.StructType); ok {
			if key, ok2 := identName(it.Key); ok2 {
				val, err := c.compileExpr(it.Value)
				if err != nil {
					if orig != nil {
						c.env = orig
					}
					return err
				}
				c.writeln(fmt.Sprintf("%s.%s = %s", itemVar, sanitizeName(key), val))
				continue
			}
		}
		keyExpr, err := c.compileExpr(it.Key)
		if err != nil {
			if orig != nil {
				c.env = orig
			}
			return err
		}
		valExpr, err := c.compileExpr(it.Value)
		if err != nil {
			if orig != nil {
				c.env = orig
			}
			return err
		}
		c.writeln(fmt.Sprintf("%s[%s] = %s", itemVar, keyExpr, valExpr))
	}

	if orig != nil {
		c.env = orig
	}
	c.indent--
	c.writeln("end")
	return nil
}
