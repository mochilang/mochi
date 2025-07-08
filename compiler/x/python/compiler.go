//go:build slow

package pycode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a subset of Mochi to Python source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	needsJSON    bool
	needsData    bool
	needsPartial bool
	needsSave    bool
	structTypes  map[string]bool
	env          *types.Env
	tmp          int
}

// New creates a new Python compiler.
func New(env *types.Env) *Compiler {
	return &Compiler{structTypes: make(map[string]bool), env: env}
}

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("__tmp%d", c.tmp)
}

// Compile converts the parsed Mochi program into Python code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needsJSON = false
	var body bytes.Buffer
	for _, s := range prog.Statements {
		if err := c.compileStmtTo(&body, s); err != nil {
			return nil, err
		}
	}
	if c.needsJSON {
		c.writeln("import json")
		c.writeln("")
	}
	if c.needsPartial {
		c.writeln("from functools import partial")
		c.writeln("")
	}
	if c.needsData {
		c.writeln("from dataclasses import dataclass")
		c.writeln("")
	}
	if c.needsSave {
		c.writeln("import sys")
		c.writeln("def _save(rows, path, opts):")
		oldIndent := c.indent
		c.indent++
		c.writeln("fmt = opts.get('format') if opts else 'jsonl'")
		c.writeln("f = sys.stdout if path in ('', '-') else open(path, 'w')")
		c.writeln("if fmt == 'jsonl':")
		c.indent++
		c.writeln("import json")
		c.writeln("for r in rows:")
		c.indent++
		c.writeln("print(json.dumps(r), file=f)")
		c.indent--
		c.indent--
		c.indent--
		c.writeln("")
		c.indent = oldIndent
	}
	c.buf.Write(body.Bytes())
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmtTo(buf *bytes.Buffer, s *parser.Statement) error {
	old := c.buf
	c.buf = *buf
	err := c.compileStmt(s)
	*buf = c.buf
	c.buf = old
	return err
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
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
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	return c.compileVarStmt(l.Name, l.Type, l.Value)
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	return c.compileVarStmt(v.Name, v.Type, v.Value)
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
	}
	c.writeln(fmt.Sprintf("def %s(%s):", fn.Name, strings.Join(params, ", ")))
	c.indent++
	for _, s := range fn.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	if len(fn.Body) == 0 {
		c.writeln("pass")
	}
	c.indent--
	return nil
}

func (c *Compiler) compileReturn(ret *parser.ReturnStmt) error {
	val, err := c.compileExpr(ret.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + val)
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	return c.compileIfChain("if", ifst)
}

func (c *Compiler) compileIfChain(keyword string, ifst *parser.IfStmt) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(keyword + " " + cond + ":")
	c.indent++
	for _, s := range ifst.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	if len(ifst.Then) == 0 {
		c.writeln("pass")
	}
	c.indent--
	if ifst.ElseIf != nil {
		if err := c.compileIfChain("elif", ifst.ElseIf); err != nil {
			return err
		}
	} else if ifst.Else != nil {
		c.writeln("else:")
		c.indent++
		for _, s := range ifst.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		if len(ifst.Else) == 0 {
			c.writeln("pass")
		}
		c.indent--
	}
	return nil
}

func (c *Compiler) compileWhile(ws *parser.WhileStmt) error {
	cond, err := c.compileExpr(ws.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + ":")
	c.indent++
	for _, s := range ws.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	if len(ws.Body) == 0 {
		c.writeln("pass")
	}
	c.indent--
	return nil
}

func (c *Compiler) compileFor(fs *parser.ForStmt) error {
	var elemType types.Type = types.AnyType{}
	if fs.RangeEnd != nil {
		start, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fs.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in range(%s, (%s)+1):", fs.Name, start, end))
		elemType = types.IntType{}
	} else {
		src, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("for %s in %s:", fs.Name, src))
		if c.env != nil {
			if lt, ok := types.TypeOfExprBasic(fs.Source, c.env).(types.ListType); ok {
				elemType = lt.Elem
			}
		}
	}
	oldEnv := c.env
	if c.env != nil {
		child := types.NewEnv(c.env)
		child.SetVar(fs.Name, elemType, true)
		c.env = child
		defer func() { c.env = oldEnv }()
	}
	c.indent++
	for _, s := range fs.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	if len(fs.Body) == 0 {
		c.writeln("pass")
	}
	c.indent--
	return nil
}

func (c *Compiler) compileTypeDecl(td *parser.TypeDecl) error {
	if len(td.Variants) > 0 {
		c.needsData = true
		c.writeln(fmt.Sprintf("class %s:", td.Name))
		c.indent++
		c.writeln("pass")
		c.indent--
		for _, v := range td.Variants {
			c.structTypes[v.Name] = true
			c.writeln("@dataclass")
			c.writeln(fmt.Sprintf("class %s(%s):", v.Name, td.Name))
			c.indent++
			if len(v.Fields) == 0 {
				c.writeln("pass")
			} else {
				for _, f := range v.Fields {
					typ, _ := c.compileType(f.Type)
					if typ == "" {
						typ = "object"
					}
					c.writeln(fmt.Sprintf("%s: %s", f.Name, typ))
				}
			}
			c.indent--
		}
		c.writeln("")
		return nil
	}
	c.needsData = true
	c.structTypes[td.Name] = true
	c.writeln("@dataclass")
	c.writeln(fmt.Sprintf("class %s:", td.Name))
	c.indent++
	if len(td.Members) == 0 {
		c.writeln("pass")
		c.indent--
		c.writeln("")
		return nil
	}
	for _, m := range td.Members {
		if m.Field != nil {
			typ, _ := c.compileType(m.Field.Type)
			if typ == "" {
				typ = "object"
			}
			c.writeln(fmt.Sprintf("%s: %s", m.Field.Name, typ))
		}
	}
	c.indent--
	c.writeln("")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
        c.writeln(fmt.Sprintf("# test %q", t.Name))
       for _, st := range t.Body {
               if err := c.compileStmt(st); err != nil {
                       return err
               }
       }
       return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln("assert " + expr)
	return nil
}

func (c *Compiler) compileVarStmt(name string, t *parser.TypeRef, val *parser.Expr) error {
	var value string
	var err error
	if val != nil {
		value, err = c.compileExpr(val)
		if err != nil {
			return err
		}
	} else {
		value = "None"
	}
	typ := ""
	if t != nil && t.Simple != nil {
		typ = *t.Simple
	}
	if typ != "" {
		c.writeln(fmt.Sprintf("%s: %s = %s", name, typ, value))
	} else {
		c.writeln(fmt.Sprintf("%s = %s", name, value))
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	target := a.Name
	for _, idx := range a.Index {
		if idx.Colon != nil {
			return fmt.Errorf("complex indexing not supported")
		}
		if idx.Start == nil {
			return fmt.Errorf("empty index")
		}
		iv, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		target += fmt.Sprintf("[%s]", iv)
	}
	for _, f := range a.Field {
		target += "." + f.Name
	}
	c.writeln(fmt.Sprintf("%s = %s", target, val))
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
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		opStr := op.Op
		switch opStr {
		case "&&":
			opStr = "and"
		case "||":
			opStr = "or"
		case "union":
			if op.All {
				res = fmt.Sprintf("(%s + %s)", res, r)
				continue
			}
			res = fmt.Sprintf("list(set(%s) | set(%s))", res, r)
			continue
		case "except":
			res = fmt.Sprintf("[x for x in %s if x not in %s]", res, r)
			continue
		case "intersect":
			res = fmt.Sprintf("[x for x in %s if x in set(%s)]", res, r)
			continue
		}
		res = fmt.Sprintf("%s %s %s", res, opStr, r)
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
			val = "not " + val
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
	for idx, op := range p.Ops {
		switch {
		case op.Cast != nil:
			typ, err := c.compileType(op.Cast.Type)
			if err != nil {
				return "", err
			}
			if c.structTypes[typ] {
				val = fmt.Sprintf("%s(**%s)", typ, val)
			} else {
				val = fmt.Sprintf("%s(%s)", typ, val)
			}
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
				start := ""
				end := ""
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				val = fmt.Sprintf("%s[%s:%s]", val, start, end)
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("empty index")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				val = fmt.Sprintf("%s[%s]", val, idx)
			}
		case op.Field != nil:
			if c.env != nil {
				prefix := &parser.PostfixExpr{Target: p.Target, Ops: p.Ops[:idx]}
				u := &parser.Unary{Value: prefix}
				t := types.TypeOfPostfixBasic(u, c.env)
				if types.IsMapType(t) || types.IsMapPostfix(prefix, c.env) {
					val = fmt.Sprintf("%s[%q]", val, op.Field.Name)
				} else {
					val = fmt.Sprintf("%s.%s", val, op.Field.Name)
				}
			} else {
				val = fmt.Sprintf("%s.%s", val, op.Field.Name)
			}
		case op.Call != nil:
			parts := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				parts[i] = s
			}
			if strings.HasSuffix(val, ".contains") && len(parts) == 1 {
				val = fmt.Sprintf("%s in %s", parts[0], strings.TrimSuffix(val, ".contains"))
			} else {
				val = fmt.Sprintf("%s(%s)", val, strings.Join(parts, ", "))
			}
		default:
			return "", fmt.Errorf("unsupported postfix at line %d", p.Target.Pos.Line)
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Call != nil:
		return c.compileCall(p.Call)
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
			k, err := c.compileMapKey(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", k, v)
		}
		return "{" + strings.Join(parts, ", ") + "}", nil
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", expr), nil
	case p.Selector != nil:
		name := p.Selector.Root
		if c.env != nil {
			t, err := c.env.GetVar(p.Selector.Root)
			if err == nil {
				for _, field := range p.Selector.Tail {
					if mt, ok := t.(types.MapType); ok && types.IsStringType(mt.Key) {
						name += fmt.Sprintf("[%q]", field)
						t = mt.Value
					} else {
						name += "." + field
						if st, ok := t.(types.StructType); ok {
							if ft, ok2 := st.Fields[field]; ok2 {
								t = ft
							} else {
								t = types.AnyType{}
							}
						} else {
							t = types.AnyType{}
						}
					}
				}
				return name, nil
			}
		}
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return name, nil
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
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("%s + [%s]", args[0], args[1]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(sum(%s) / len(%s))", args[0], args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("len(%s)", args[0]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		return fmt.Sprintf("list(%s.values())", args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("%s[%s:%s]", args[0], args[1], args[2]), nil
	case "json":
		c.needsJSON = true
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		return fmt.Sprintf("json.dumps(%s)", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		return fmt.Sprintf("(len(%s) > 0)", args[0]), nil
	default:
		if c.env != nil {
			if fn, ok := c.env.GetFunc(call.Func); ok {
				if len(call.Args) < len(fn.Params) {
					c.needsPartial = true
					return fmt.Sprintf("partial(%s, %s)", call.Func, strings.Join(args, ", ")), nil
				}
			}
		}
		return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
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
		return fmt.Sprintf("lambda %s: %s", strings.Join(params, ", "), body), nil
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
	loops := []string{fmt.Sprintf("%s in %s", q.Var, src)}
	for _, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		loops = append(loops, fmt.Sprintf("%s in %s", f.Var, s))
	}
	loopStr := strings.Join(loops, " for ")
	cond := ""
	if q.Where != nil {
		cnd, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		cond = " if " + cnd
	}
	selCall := q.Select.Binary.Left.Value.Target.Call
	if selCall != nil && selCall.Func == "sum" && len(selCall.Args) == 1 {
		arg, err := c.compileExpr(selCall.Args[0])
		if err != nil {
			return "", err
		}
		expr := fmt.Sprintf("sum(%s for %s%s)", arg, loopStr, cond)
		return expr, nil
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	expr := fmt.Sprintf("[%s for %s%s]", sel, loopStr, cond)
	if q.Sort != nil {
		keyExpr := q.Sort
		if m, ok := isMapLiteral(keyExpr); ok {
			parts := make([]string, len(m.Items))
			for i, it := range m.Items {
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				parts[i] = v
			}
			key := "(" + strings.Join(parts, ", ") + ")"
			expr = fmt.Sprintf("sorted([%s for %s%s], key=lambda x: %s)", sel, loopStr, cond, key)
		} else {
			key, err := c.compileExpr(keyExpr)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("[x[1] for x in sorted([( %s, %s ) for %s%s], key=lambda x: x[0])]", key, sel, loopStr, cond)
		}
	}
	if q.Skip != nil || q.Take != nil {
		start := "0"
		if q.Skip != nil {
			start, err = c.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
		}
		end := ""
		if q.Take != nil {
			end, err = c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
			if q.Skip != nil {
				end = fmt.Sprintf("(%s)+(%s)", start, end)
			}
		}
		expr = fmt.Sprintf("%s[%s:%s]", expr, start, end)
	}
	if q.Distinct {
		expr = fmt.Sprintf("list(dict.fromkeys(%s))", expr)
	}
	return expr, nil
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
		return fmt.Sprintf("(%s if %s else %s)", thenExpr, cond, elseExpr), nil
	}
	elseCode := "None"
	if ix.Else != nil {
		elseCode, err = c.compileExpr(ix.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("(%s if %s else %s)", thenExpr, cond, elseCode), nil
}

func (c *Compiler) compileMatchExpr(mx *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(mx.Target)
	if err != nil {
		return "", err
	}
	elseExpr := "None"
	expr := ""
	for i := len(mx.Cases) - 1; i >= 0; i-- {
		patStr, patIsIdent := c.simpleIdentifier(mx.Cases[i].Pattern)
		res, err := c.compileExpr(mx.Cases[i].Result)
		if err != nil {
			return "", err
		}
		if patIsIdent && patStr == "_" {
			elseExpr = res
			continue
		}
		pat, err := c.compileExpr(mx.Cases[i].Pattern)
		if err != nil {
			return "", err
		}
		if expr == "" {
			expr = fmt.Sprintf("(%s if %s == %s else %s)", res, target, pat, elseExpr)
		} else {
			expr = fmt.Sprintf("(%s if %s == %s else %s)", res, target, pat, expr)
		}
	}
	if expr == "" {
		expr = elseExpr
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
	opts := "{}"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.needsSave = true
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileStructLiteral(sl *parser.StructLiteral) (string, error) {
	fields := make([]string, len(sl.Fields))
	for i, f := range sl.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s=%s", f.Name, v)
	}
	return fmt.Sprintf("%s(%s)", sl.Name, strings.Join(fields, ", ")), nil
}

func (c *Compiler) compileMapKey(e *parser.Expr) (string, error) {
	if name, ok := c.simpleIdentifier(e); ok {
		return fmt.Sprintf("%q", name), nil
	}
	return c.compileExpr(e)
}

func isMapLiteral(e *parser.Expr) (*parser.MapLiteral, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return nil, false
	}
	if u.Value.Target != nil && u.Value.Target.Map != nil {
		return u.Value.Target.Map, true
	}
	return nil, false
}

func isStructLiteral(e *parser.Expr) (*parser.StructLiteral, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return nil, false
	}
	if u.Value.Target != nil && u.Value.Target.Struct != nil {
		return u.Value.Target.Struct, true
	}
	return nil, false
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
			return "True"
		}
		return "False"
	case l.Null:
		return "None"
	default:
		return "None"
	}
}

func (c *Compiler) compileType(t *parser.TypeRef) (string, error) {
	if t == nil || t.Simple == nil {
		return "", fmt.Errorf("unsupported type")
	}
	switch *t.Simple {
	case "int":
		return "int", nil
	case "string":
		return "str", nil
	case "float":
		return "float", nil
	case "bool":
		return "bool", nil
	default:
		return *t.Simple, nil
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
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
