//go:build slow

package c

import (
	"bytes"
	"fmt"
	"strings"

	meta "mochi/compiler/meta"
	"mochi/parser"
	"mochi/types"
)

// Compiler converts a small subset of Mochi into C source code.
// Only basic statements and expressions used in simple examples are
// supported at the moment. Unsupported constructs return an error so
// the tests can record the failure.
type Compiler struct {
	buf      bytes.Buffer
	prelude  bytes.Buffer
	indent   int
	env      *types.Env
	vars     map[string]string
	lens     map[string]int
	listVals map[string][]string

	mapVals map[string][]string

	mapTypes map[string]string

	needsContains  bool
	needsPrintList bool
	needsAvg       bool
	needsSum       bool
	needsMin       bool
	needsMax       bool
	needsSubstr    bool

	needsConcat bool
	needsToStr  bool
	needsNow    bool

	needsSliceList bool

	needsGetSI      bool
	needsGetIS      bool
	needsContainsSI bool
	needsContainsIS bool

	aliases     map[string]string
	lambdaCount int
	tmp         int
	lastType    string
	returnType  string
	funRet      map[string]string
}

// New creates a new Compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:      env,
		vars:     make(map[string]string),
		lens:     make(map[string]int),
		listVals: make(map[string][]string),
		mapVals:  make(map[string][]string),
		mapTypes: make(map[string]string),
		aliases:  make(map[string]string),
		funRet:   make(map[string]string),
	}
}

func (c *Compiler) newTmp() string {
	c.tmp++
	return fmt.Sprintf("__tmp%d", c.tmp)
}

// Compile translates the parsed program into C code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.prelude.Reset()
	c.indent = 0

	var typeBuf bytes.Buffer
	var funcBuf bytes.Buffer
	var body bytes.Buffer
	for _, st := range p.Statements {
		if st.Type != nil {
			old := c.buf
			c.buf = typeBuf
			if err := c.compileTypeDecl(st.Type); err != nil {
				c.buf = old
				return nil, err
			}
			c.writeln("")
			typeBuf = c.buf
			c.buf = old
		}
	}
	for _, st := range p.Statements {
		if st.Fun != nil {
			old := c.buf
			c.buf = funcBuf
			if err := c.compileFun(st.Fun); err != nil {
				c.buf = old
				return nil, err
			}
			c.writeln("")
			funcBuf = c.buf
			c.buf = old
		}
	}

	for _, st := range p.Statements {
		if st.Fun != nil || st.Type != nil {
			continue
		}
		oldBuf := c.buf
		c.buf = body
		if err := c.compileStmt(st); err != nil {
			c.buf = oldBuf
			return nil, err
		}
		body = c.buf
		c.buf = oldBuf
	}

	c.buf.Reset()
	c.buf.Write(meta.Header("//"))
	c.writeln("#include <stdio.h>")
	c.writeln("#include <stdbool.h>")
	c.writeln("#include <stdlib.h>")
	c.writeln("#include <string.h>")
	if c.needsNow {
		c.writeln("#include <time.h>")
	}
	c.writeln("")

	c.buf.Write(typeBuf.Bytes())
	if typeBuf.Len() > 0 {
		c.writeln("")
	}

	c.buf.Write(c.prelude.Bytes())
	if c.prelude.Len() > 0 {
		c.writeln("")
	}

	c.buf.Write(funcBuf.Bytes())
	if funcBuf.Len() > 0 {
		c.writeln("")
	}

	c.writeHelpers()

	c.writeln("int main() {")
	c.indent++
	c.buf.Write(body.Bytes())
	c.writeln("return 0;")
	c.indent--
	c.writeln("}")

	return c.buf.Bytes(), nil
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) compileTypeDecl(td *parser.TypeDecl) error {
	// Only simple struct types without methods are supported.
	if len(td.Members) == 0 {
		return fmt.Errorf("unsupported type decl")
	}
	c.writeIndent()
	fmt.Fprintf(&c.buf, "typedef struct {\n")
	c.indent++
	for _, m := range td.Members {
		if m.Field == nil {
			return fmt.Errorf("unsupported type member")
		}
		ft, _ := c.compileType(m.Field.Type)
		c.writeIndent()
		fmt.Fprintf(&c.buf, "%s %s;\n", ft, m.Field.Name)
	}
	c.indent--
	c.writeln("} " + td.Name + ";")
	return nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	oldVars := c.vars
	oldRet := c.returnType
	c.vars = make(map[string]string)
	for _, p := range fn.Params {
		pt, _ := c.compileType(p.Type)
		c.vars[p.Name] = pt
	}

	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body
	c.returnType = ""
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			c.buf = oldBuf
			c.vars = oldVars
			c.returnType = oldRet
			return err
		}
	}
	body = c.buf
	c.buf = oldBuf

	ret := c.returnType
	if ret == "" {
		ret, _ = c.compileType(fn.Return)
	}
	c.funRet[fn.Name] = ret

	c.writeIndent()
	c.buf.WriteString(ret)
	c.buf.WriteByte(' ')
	c.buf.WriteString(fn.Name)
	c.buf.WriteString("(")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		pt := c.vars[p.Name]
		c.buf.WriteString(pt)
		c.buf.WriteByte(' ')
		c.buf.WriteString(p.Name)
	}
	c.buf.WriteString(") {\n")
	c.indent++
	c.buf.Write(body.Bytes())
	c.indent--
	c.writeln("}")

	c.vars = oldVars
	c.returnType = oldRet
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		c.writeIndent()
		if lst := listLiteral(s.Let.Value); lst != nil {
			if len(lst.Elems) > 0 && listLiteral(lst.Elems[0]) != nil {
				inner := listLiteral(lst.Elems[0])
				cols := len(inner.Elems)
				rows := make([]string, len(lst.Elems))
				for i, r := range lst.Elems {
					rl := listLiteral(r)
					if rl == nil || len(rl.Elems) != cols {
						return fmt.Errorf("unsupported nested list")
					}
					elems := make([]string, cols)
					for j, e := range rl.Elems {
						s, err := c.compileExpr(e)
						if err != nil {
							return err
						}
						elems[j] = s
					}
					rows[i] = fmt.Sprintf("{%s}", strings.Join(elems, ", "))
				}
				fmt.Fprintf(&c.buf, "int %s[][ %d ] = {%s};\n", s.Let.Name, cols, strings.Join(rows, ", "))
				c.vars[s.Let.Name] = "int[][]"
			} else {
				elems := make([]string, len(lst.Elems))
				for i, e := range lst.Elems {
					s, err := c.compileExpr(e)
					if err != nil {
						return err
					}
					elems[i] = s
				}
				fmt.Fprintf(&c.buf, "int %s[] = {%s};\n", s.Let.Name, strings.Join(elems, ", "))
				c.vars[s.Let.Name] = "int[]"
				c.lens[s.Let.Name] = len(lst.Elems)
				c.listVals[s.Let.Name] = elems
			}
		} else if mp := mapLiteral(s.Let.Value); mp != nil {
			val, err := c.compileMapLiteral(mp, true)
			if err != nil {
				return err
			}
			typ := c.lastType
			entry := strings.TrimSuffix(typ, "[]")
			fmt.Fprintf(&c.buf, "%s %s[] = %s;\n", entry, s.Let.Name, val)
			c.vars[s.Let.Name] = typ
			c.lens[s.Let.Name] = len(mp.Items)
			c.mapTypes[s.Let.Name] = entry
			vals := make([]string, len(mp.Items))
			for i, it := range mp.Items {
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return err
				}
				vals[i] = v
			}
			c.mapVals[s.Let.Name] = vals
		} else {
			var val string
			var err error
			if s.Let.Value != nil {
				val, err = c.compileExpr(s.Let.Value)
				if err != nil {
					return err
				}
			}
			typ, _ := c.compileType(s.Let.Type)
			if s.Let.Type == nil && c.lastType != "" {
				typ = c.lastType
			}
			if typ == "int" && isStringLiteral(s.Let.Value) {
				typ = "const char*"
			} else if typ == "int" && s.Let.Value != nil {
				t := types.TypeOfExpr(s.Let.Value, c.env)
				if st, ok := t.(types.StructType); ok {
					typ = st.Name
				} else if types.IsStringType(t) {
					typ = "const char*"
				}
			}
			c.buf.WriteString(typ)
			c.buf.WriteByte(' ')
			c.buf.WriteString(s.Let.Name)
			if s.Let.Value != nil {
				c.buf.WriteString(" = ")
				c.buf.WriteString(val)
			}
			c.buf.WriteString(";\n")
			c.vars[s.Let.Name] = typ
		}
	case s.Var != nil:
		c.writeIndent()
		if lst := listLiteral(s.Var.Value); lst != nil {
			if len(lst.Elems) > 0 && listLiteral(lst.Elems[0]) != nil {
				inner := listLiteral(lst.Elems[0])
				cols := len(inner.Elems)
				rows := make([]string, len(lst.Elems))
				for i, r := range lst.Elems {
					rl := listLiteral(r)
					if rl == nil || len(rl.Elems) != cols {
						return fmt.Errorf("unsupported nested list")
					}
					elems := make([]string, cols)
					for j, e := range rl.Elems {
						s, err := c.compileExpr(e)
						if err != nil {
							return err
						}
						elems[j] = s
					}
					rows[i] = fmt.Sprintf("{%s}", strings.Join(elems, ", "))
				}
				fmt.Fprintf(&c.buf, "int %s[][ %d ] = {%s};\n", s.Var.Name, cols, strings.Join(rows, ", "))
				c.vars[s.Var.Name] = "int[][]"
			} else {
				elems := make([]string, len(lst.Elems))
				for i, e := range lst.Elems {
					s, err := c.compileExpr(e)
					if err != nil {
						return err
					}
					elems[i] = s
				}
				fmt.Fprintf(&c.buf, "int %s[] = {%s};\n", s.Var.Name, strings.Join(elems, ", "))
				c.vars[s.Var.Name] = "int[]"
				c.lens[s.Var.Name] = len(lst.Elems)
				c.listVals[s.Var.Name] = elems
			}
		} else if mp := mapLiteral(s.Var.Value); mp != nil {
			val, err := c.compileMapLiteral(mp, true)
			if err != nil {
				return err
			}
			typ := c.lastType
			entry := strings.TrimSuffix(typ, "[]")
			fmt.Fprintf(&c.buf, "%s %s[] = %s;\n", entry, s.Var.Name, val)
			c.vars[s.Var.Name] = typ
			c.lens[s.Var.Name] = len(mp.Items)
			c.mapTypes[s.Var.Name] = entry
			vals := make([]string, len(mp.Items))
			for i, it := range mp.Items {
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return err
				}
				vals[i] = v
			}
			c.mapVals[s.Var.Name] = vals
		} else {
			var val string
			var err error
			if s.Var.Value != nil {
				val, err = c.compileExpr(s.Var.Value)
				if err != nil {
					return err
				}
			}
			typ, _ := c.compileType(s.Var.Type)
			if s.Var.Type == nil && c.lastType != "" {
				typ = c.lastType
			}
			if typ == "int" && isStringLiteral(s.Var.Value) {
				typ = "const char*"
			} else if typ == "int" && s.Var.Value != nil {
				t := types.TypeOfExpr(s.Var.Value, c.env)
				if st, ok := t.(types.StructType); ok {
					typ = st.Name
				} else if types.IsStringType(t) {
					typ = "const char*"
				}
			}
			c.buf.WriteString(typ)
			c.buf.WriteByte(' ')
			c.buf.WriteString(s.Var.Name)
			if s.Var.Value != nil {
				c.buf.WriteString(" = ")
				c.buf.WriteString(val)
			}
			c.buf.WriteString(";\n")
			c.vars[s.Var.Name] = typ
		}
	case s.Assign != nil:
		c.writeIndent()
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		target := s.Assign.Name
		if len(s.Assign.Field) > 0 {
			return fmt.Errorf("field assignment not supported")
		}
		for _, idx := range s.Assign.Index {
			i, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			target += fmt.Sprintf("[%s]", i)
		}
		c.buf.WriteString(target)
		c.buf.WriteString(" = ")
		c.buf.WriteString(val)
		c.buf.WriteString(";\n")
	case s.Expr != nil:
		c.writeIndent()
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.buf.WriteString(expr)
		c.buf.WriteString(";\n")
	case s.While != nil:
		cond, err := c.compileExpr(s.While.Cond)
		if err != nil {
			return err
		}
		c.writeIndent()
		c.buf.WriteString("while (")
		c.buf.WriteString(cond)
		c.buf.WriteString(") {\n")
		c.indent++
		for _, st := range s.While.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	case s.If != nil:
		if err := c.compileIf(s.If, "if"); err != nil {
			return err
		}
	case s.For != nil:
		if err := c.compileFor(s.For); err != nil {
			return err
		}
	case s.Return != nil:
		c.writeIndent()
		if s.Return.Value != nil {
			val, err := c.compileExpr(s.Return.Value)
			if err != nil {
				return err
			}
			c.buf.WriteString("return ")
			c.buf.WriteString(val)
			c.buf.WriteString(";\n")
			if c.lastType != "" {
				c.returnType = c.lastType
			}
		} else {
			c.buf.WriteString("return;\n")
		}
	case s.Break != nil:
		c.writeIndent()
		c.buf.WriteString("break;\n")
	case s.Continue != nil:
		c.writeIndent()
		c.buf.WriteString("continue;\n")
	case s.Fun != nil:
		fe := &parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body}
		oldRet := c.returnType
		val, err := c.compileFunExpr(fe)
		c.returnType = oldRet
		if err != nil {
			return err
		}
		typ := c.lastType
		c.writeIndent()
		fmt.Fprintf(&c.buf, "%s %s = %s;\n", typ, s.Fun.Name, val)
		c.vars[s.Fun.Name] = typ
		c.lastType = ""
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("empty expr")
	}
	if e.Binary != nil {
		left, err := c.compileUnary(e.Binary.Left)
		if err != nil {
			return "", err
		}
		out := left
		leftAST := e.Binary.Left
		for _, op := range e.Binary.Right {
			rhs, err := c.compilePostfix(op.Right)
			if err != nil {
				return "", err
			}
			if op.Op == "in" {
				expr, err := c.compileInOp(leftAST, op.Right, out, rhs)
				if err != nil {
					return "", err
				}
				out = expr
			} else if op.Op == "+" && (c.unaryIsString(leftAST) || c.postfixIsString(op.Right)) {
				c.needsConcat = true
				out = fmt.Sprintf("str_concat(%s, %s)", out, rhs)
			} else if (op.Op == "==" || op.Op == "!=" || op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=") && (c.unaryIsString(leftAST) || c.postfixIsString(op.Right)) {
				out = fmt.Sprintf("(strcmp(%s, %s) %s 0)", out, rhs, op.Op)
			} else {
				out = fmt.Sprintf("(%s %s %s)", out, op.Op, rhs)
			}
			leftAST = &parser.Unary{Value: op.Right}
		}
		return out, nil
	}
	return "", fmt.Errorf("unsupported expression at line %d", e.Pos.Line)
}

func (c *Compiler) compileIf(ifst *parser.IfStmt, prefix string) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeIndent()
	c.buf.WriteString(prefix)
	c.buf.WriteString(" (")
	c.buf.WriteString(cond)
	c.buf.WriteString(") {\n")
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if ifst.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(ifst.ElseIf, "if")
	}
	if len(ifst.Else) > 0 {
		c.buf.WriteString(" else {\n")
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}\n")
	} else {
		c.buf.WriteByte('\n')
	}
	return nil
}

func (c *Compiler) compileFor(fr *parser.ForStmt) error {
	if fr.RangeEnd != nil {
		start, err := c.compileExpr(fr.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fr.RangeEnd)
		if err != nil {
			return err
		}
		c.writeIndent()
		fmt.Fprintf(&c.buf, "for (int %s = %s; %s < %s; %s++) {\n", fr.Name, start, fr.Name, end, fr.Name)
		c.indent++
		for _, st := range fr.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}

	if fr.Source.Binary == nil {
		return fmt.Errorf("unsupported for source")
	}
	srcU := fr.Source.Binary.Left
	srcP := srcU.Value
	arrExpr, err := c.compilePostfix(srcP)
	if err != nil {
		return err
	}
	t := types.TypeOfExpr(fr.Source, c.env)
	if mt, ok := t.(types.MapType); ok {
		var lenExpr string
		if srcP.Target.Map != nil {
			lenExpr = fmt.Sprintf("%d", len(srcP.Target.Map.Items))
		} else if srcP.Target.Selector != nil {
			name := srcP.Target.Selector.Root
			if l, ok := c.lens[name]; ok {
				lenExpr = fmt.Sprintf("%d", l)
			} else {
				return fmt.Errorf("unknown length for %s", name)
			}
		}
		idx := "__i"
		keyType := "int"
		if _, ok := mt.Key.(types.StringType); ok {
			keyType = "const char*"
		}
		c.writeIndent()
		fmt.Fprintf(&c.buf, "for (int %s = 0; %s < %s; %s++) {\n", idx, idx, lenExpr, idx)
		c.indent++
		c.writeIndent()
		fmt.Fprintf(&c.buf, "%s %s = %s[%s].key;\n", keyType, fr.Name, arrExpr, idx)
		oldVar := c.vars[fr.Name]
		c.vars[fr.Name] = keyType
		for _, st := range fr.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		if oldVar == "" {
			delete(c.vars, fr.Name)
		} else {
			c.vars[fr.Name] = oldVar
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	var lenExpr string
	if srcP.Target.List != nil {
		lenExpr = fmt.Sprintf("%d", len(srcP.Target.List.Elems))
	} else {
		lenExpr = fmt.Sprintf("sizeof(%s)/sizeof(%s[0])", arrExpr, arrExpr)
	}
	idx := "__i"
	c.writeIndent()
	fmt.Fprintf(&c.buf, "for (int %s = 0; %s < %s; %s++) {\n", idx, idx, lenExpr, idx)
	c.indent++
	c.writeIndent()
	fmt.Fprintf(&c.buf, "int %s = %s[%s];\n", fr.Name, arrExpr, idx)
	for _, st := range fr.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
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
	for i, op := range p.Ops {
		switch {
		case op.Field != nil:
			expr = fmt.Sprintf("%s.%s", expr, op.Field.Name)
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			if strings.HasSuffix(expr, ".contains") && len(args) == 1 {
				base := strings.TrimSuffix(expr, ".contains")
				expr = fmt.Sprintf("(strstr(%s, %s) != NULL)", base, args[0])
			} else if id := primaryIdent(p.Target); id != "" {
				if t, ok := c.vars[id]; ok && strings.HasPrefix(t, "lambda") {
					expr = fmt.Sprintf("%s_apply(&%s, %s)", t, id, strings.Join(args, ", "))
				} else {
					expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
				}
			} else {
				expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
			}
		case op.Cast != nil:
			typ, err := c.compileType(op.Cast.Type)
			if err != nil {
				return "", err
			}
			if typ == "int" && strings.HasPrefix(expr, "\"") {
				expr = fmt.Sprintf("atoi(%s)", expr)
			} else if _, ok := c.env.GetStruct(typ); ok && strings.HasPrefix(strings.TrimSpace(expr), "{") {
				expr = fmt.Sprintf("(%s)%s", typ, expr)
			} else {
				expr = fmt.Sprintf("(%s)(%s)", typ, expr)
			}
		case op.Index != nil:
			if op.Index.Colon != nil {
				if op.Index.Start == nil || op.Index.End == nil {
					return "", fmt.Errorf("complex indexing not supported")
				}
				startStr, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				endStr, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				prefix := &parser.PostfixExpr{Target: p.Target, Ops: p.Ops[:i]}
				t := types.TypeOfPostfix(prefix, c.env)
				if types.IsStringType(t) {
					c.needsSubstr = true
					expr = fmt.Sprintf("substr(%s, %s, %s)", expr, startStr, endStr)
				} else if types.IsListType(t) {
					c.needsSliceList = true
					expr = fmt.Sprintf("list_slice(%s, %s, %s)", expr, startStr, endStr)
					c.lastType = "int[]"
				} else {
					return "", fmt.Errorf("slice unsupported")
				}
				continue
			}
			if op.Index.Start == nil {
				return "", fmt.Errorf("complex indexing not supported")
			}
			idxStr, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			prefix := &parser.PostfixExpr{Target: p.Target, Ops: p.Ops[:i]}
			t := types.TypeOfPostfix(prefix, c.env)
			if mt, ok := t.(types.MapType); ok {
				var length string
				if prefix.Target.Map != nil {
					length = fmt.Sprintf("%d", len(prefix.Target.Map.Items))
				} else if prefix.Target.Selector != nil {
					name := prefix.Target.Selector.Root
					if l, ok := c.lens[name]; ok {
						length = fmt.Sprintf("%d", l)
					} else {
						return "", fmt.Errorf("unknown length for %s", name)
					}
				}
				if _, ok := mt.Key.(types.StringType); ok {
					c.needsGetSI = true
					expr = fmt.Sprintf("map_get_str_int(%s, %s, %s)", expr, length, idxStr)
				} else {
					c.needsGetIS = true
					expr = fmt.Sprintf("map_get_int_str(%s, %s, %s)", expr, length, idxStr)
				}
			} else {
				expr = fmt.Sprintf("%s[%s]", expr, idxStr)
			}
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Selector != nil:
		name := p.Selector.Root
		if a, ok := c.aliases[name]; ok {
			name = a
		}
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return name, nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "(int[]){" + strings.Join(elems, ", ") + "}", nil
	case p.Map != nil:
		return c.compileMapLiteral(p.Map, false)
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Group != nil:
		return c.compileExpr(p.Group)
	default:
		return "", fmt.Errorf("unsupported primary at line %d", p.Pos.Line)
	}
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
	var elseExpr string
	if ix.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ix.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ix.Else != nil {
		elseExpr, err = c.compileExpr(ix.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "0"
	}
	return fmt.Sprintf("(%s ? %s : %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral, forceMap bool) (string, error) {
	if len(m.Items) == 0 {
		return "{}", nil
	}
	simple := true
	keys := make([]string, len(m.Items))
	for i, item := range m.Items {
		name := simpleMapKey(item.Key)
		if name == "" {
			simple = false
			break
		}
		keys[i] = name
	}
	if simple && !forceMap {
		fields := make([]string, len(m.Items))
		for i, item := range m.Items {
			val, err := c.compileExpr(item.Value)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf(".%s = %s", keys[i], val)
		}
		return "{" + strings.Join(fields, ", ") + "}", nil
	}

	keyT := types.TypeOfExpr(m.Items[0].Key, c.env)
	valT := types.TypeOfExpr(m.Items[0].Value, c.env)
	var entryType string
	if _, ok := keyT.(types.StringType); ok {
		if _, ok := valT.(types.IntType); ok {
			entryType = "EntrySI"
			c.needsGetSI = true
		}
	} else if _, ok := keyT.(types.IntType); ok {
		if _, ok := valT.(types.StringType); ok {
			entryType = "EntryIS"
			c.needsGetIS = true
		}
	}
	if entryType == "" {
		return "", fmt.Errorf("unsupported map types")
	}
	items := make([]string, len(m.Items))
	for i, item := range m.Items {
		k, err := c.compileExpr(item.Key)
		if err != nil {
			return "", err
		}
		v, err := c.compileExpr(item.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("{%s, %s}", k, v)
	}
	c.lastType = entryType + "[]"
	return "{" + strings.Join(items, ", ") + "}", nil
}

func (c *Compiler) compileStructLiteral(sl *parser.StructLiteral) (string, error) {
	fields := make([]string, len(sl.Fields))
	for i, f := range sl.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf(".%s = %s", f.Name, v)
	}
	c.lastType = sl.Name
	return fmt.Sprintf("(%s){ %s }", sl.Name, strings.Join(fields, ", ")), nil
}

func simpleMapKey(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if u.Value == nil || len(u.Ops) > 0 || len(u.Value.Ops) > 0 {
		return ""
	}
	if lit := u.Value.Target.Lit; lit != nil && lit.Str != nil {
		return *lit.Str
	}
	if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
		return sel.Root
	}
	return ""
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	if call.Func == "exists" && len(call.Args) == 1 {
		if q := getQueryArg(call.Args[0]); q != nil {
			return c.compileExistsQuery(q)
		}
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	if t, ok := c.vars[call.Func]; ok && strings.HasPrefix(t, "lambda") {
		return fmt.Sprintf("%s_apply(&%s, %s)", t, call.Func, strings.Join(args, ", ")), nil
	}
	if call.Func == "print" {
		switch len(args) {
		case 1:
			arg := args[0]
			if call.Args[0].Binary != nil {
				u := call.Args[0].Binary.Left
				if len(call.Args[0].Binary.Right) == 0 && len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 0 {
					if sub := u.Value.Target.Call; sub != nil && sub.Func == "values" {
						if mp := mapLiteral(sub.Args[0]); mp != nil {
							arr, err := c.compileCall(sub)
							if err != nil {
								return "", err
							}
							c.needsPrintList = true
							return fmt.Sprintf("print_list(%s, %d)", arr, len(mp.Items)), nil
						}
						if name := simpleIdent(sub.Args[0]); name != "" {
							if vals, ok := c.mapVals[name]; ok {
								arr, err := c.compileCall(sub)
								if err != nil {
									return "", err
								}
								c.needsPrintList = true
								return fmt.Sprintf("print_list(%s, %d)", arr, len(vals)), nil
							}
						}
					}
				}
			}
			if call.Args[0].Binary != nil {
				u := call.Args[0].Binary.Left
				if len(call.Args[0].Binary.Right) == 0 && len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 0 {
					if ap := u.Value.Target.Call; ap != nil && ap.Func == "append" {
						if len(ap.Args) == 2 {
							base := ap.Args[0]
							extra, err := c.compileExpr(ap.Args[1])
							if err != nil {
								return "", err
							}
							if lst := listLiteral(base); lst != nil {
								elems := make([]string, len(lst.Elems))
								for i, e := range lst.Elems {
									s, err := c.compileExpr(e)
									if err != nil {
										return "", err
									}
									elems[i] = s
								}
								arr := fmt.Sprintf("(int[]){%s, %s}", strings.Join(elems, ", "), extra)
								c.needsPrintList = true
								return fmt.Sprintf("print_list(%s, %d)", arr, len(lst.Elems)+1), nil
							}
							if name := simpleIdent(base); name != "" {
								if vals, ok := c.listVals[name]; ok {
									elems := append(append([]string{}, vals...), extra)
									arr := fmt.Sprintf("(int[]){%s}", strings.Join(elems, ", "))
									c.needsPrintList = true
									return fmt.Sprintf("print_list(%s, %d)", arr, len(elems)), nil
								}
							}
						}
					}
				}
			}
			if lst := listLiteral(call.Args[0]); lst != nil {
				c.needsPrintList = true
				elems := make([]string, len(lst.Elems))
				for i, e := range lst.Elems {
					s, err := c.compileExpr(e)
					if err != nil {
						return "", err
					}
					elems[i] = s
				}
				arr := fmt.Sprintf("(int[]){%s}", strings.Join(elems, ", "))
				return fmt.Sprintf("print_list(%s, %d)", arr, len(lst.Elems)), nil
			}
			if name := simpleIdent(call.Args[0]); name != "" && c.vars[name] == "int[]" {
				if l, ok := c.lens[name]; ok {
					c.needsPrintList = true
					return fmt.Sprintf("print_list(%s, %d)", name, l), nil
				}
			}
			if c.exprIsList(call.Args[0]) {
				length, err := c.compileLenExpr(call.Args[0])
				if err == nil {
					c.needsPrintList = true
					return fmt.Sprintf("print_list(%s, %s)", arg, length), nil
				}
			}
			if isStringIndex(call.Args[0], c) {
				return fmt.Sprintf("printf(\"%%c\\n\", %s)", arg), nil
			}
			if strings.HasPrefix(arg, "\"") || c.exprIsString(call.Args[0]) {
				return fmt.Sprintf("printf(\"%s\\n\", %s)", "%s", arg), nil
			}
			return fmt.Sprintf("printf(\"%s\\n\", %s)", "%d", arg), nil
		case 2:
			if strings.HasPrefix(args[0], "\"") {
				return fmt.Sprintf("printf(\"%%s %%d\\n\", %s, %s)", args[0], args[1]), nil
			}
			str0 := c.exprIsString(call.Args[0])
			str1 := c.exprIsString(call.Args[1])
			switch {
			case str0 && str1:
				return fmt.Sprintf("printf(\"%%s %%s\\n\", %s, %s)", args[0], args[1]), nil
			case str0 && !str1:
				return fmt.Sprintf("printf(\"%%s %%d\\n\", %s, %s)", args[0], args[1]), nil
			case !str0 && str1:
				return fmt.Sprintf("printf(\"%%d %%s\\n\", %s, %s)", args[0], args[1]), nil
			default:
				return fmt.Sprintf("printf(\"%%d %%d\\n\", %s, %s)", args[0], args[1]), nil
			}
		default:
			return "", fmt.Errorf("print expects one or two arguments")
		}
	} else if call.Func == "len" {
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return c.compileLenExpr(call.Args[0])
	} else if call.Func == "append" {
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		if lst := listLiteral(call.Args[0]); lst != nil {
			elems := make([]string, len(lst.Elems))
			for i, e := range lst.Elems {
				s, err := c.compileExpr(e)
				if err != nil {
					return "", err
				}
				elems[i] = s
			}
			arr := fmt.Sprintf("(int[]){%s, %s}", strings.Join(elems, ", "), args[1])
			// caller must know length
			return arr, nil
		} else if name := simpleIdent(call.Args[0]); name != "" {
			if vals, ok := c.listVals[name]; ok {
				elems := append(append([]string{}, vals...), args[1])
				arr := fmt.Sprintf("(int[]){%s}", strings.Join(elems, ", "))
				return arr, nil
			}
		}
		return "", fmt.Errorf("append unsupported")
	} else if call.Func == "avg" {
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		c.needsAvg = true
		var length string
		if lst := listLiteral(call.Args[0]); lst != nil {
			length = fmt.Sprintf("%d", len(lst.Elems))
		} else if name := simpleIdent(call.Args[0]); name != "" {
			if l, ok := c.lens[name]; ok {
				length = fmt.Sprintf("%d", l)
			}
		}
		if length == "" {
			return "", fmt.Errorf("avg unsupported")
		}
		return fmt.Sprintf("avg(%s, %s)", args[0], length), nil
	} else if call.Func == "count" {
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		if lst := listLiteral(call.Args[0]); lst != nil {
			return fmt.Sprintf("%d", len(lst.Elems)), nil
		}
		if name := simpleIdent(call.Args[0]); name != "" {
			if l, ok := c.lens[name]; ok {
				return fmt.Sprintf("%d", l), nil
			}
		}
		return "", fmt.Errorf("count unsupported")
	} else if call.Func == "sum" {
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		c.needsSum = true
		var length string
		if lst := listLiteral(call.Args[0]); lst != nil {
			length = fmt.Sprintf("%d", len(lst.Elems))
		} else if name := simpleIdent(call.Args[0]); name != "" {
			if l, ok := c.lens[name]; ok {
				length = fmt.Sprintf("%d", l)
			}
		}
		if length == "" {
			return "", fmt.Errorf("sum unsupported")
		}
		return fmt.Sprintf("sum(%s, %s)", args[0], length), nil
	} else if call.Func == "min" {
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		c.needsMin = true
		var length string
		if lst := listLiteral(call.Args[0]); lst != nil {
			length = fmt.Sprintf("%d", len(lst.Elems))
		} else if name := simpleIdent(call.Args[0]); name != "" {
			if l, ok := c.lens[name]; ok {
				length = fmt.Sprintf("%d", l)
			}
		}
		if length == "" {
			return "", fmt.Errorf("min unsupported")
		}
		return fmt.Sprintf("min_val(%s, %s)", args[0], length), nil
	} else if call.Func == "max" {
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		c.needsMax = true
		var length string
		if lst := listLiteral(call.Args[0]); lst != nil {
			length = fmt.Sprintf("%d", len(lst.Elems))
		} else if name := simpleIdent(call.Args[0]); name != "" {
			if l, ok := c.lens[name]; ok {
				length = fmt.Sprintf("%d", l)
			}
		}
		if length == "" {
			return "", fmt.Errorf("max unsupported")
		}
		return fmt.Sprintf("max_val(%s, %s)", args[0], length), nil
	} else if call.Func == "str" {
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		t := types.TypeOfExpr(call.Args[0], c.env)
		if types.IsStringType(t) {
			return args[0], nil
		}
		c.needsToStr = true
		return fmt.Sprintf("to_str(%s)", args[0]), nil
	} else if call.Func == "substring" {
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		c.needsSubstr = true
		return fmt.Sprintf("substr(%s, %s, %s)", args[0], args[1], args[2]), nil
	} else if call.Func == "now" {
		if len(args) != 0 {
			return "", fmt.Errorf("now expects no args")
		}
		c.needsNow = true
		return "_now()", nil
	} else if call.Func == "json" {
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		if ml := mapLiteral(call.Args[0]); ml != nil {
			var fmtStr strings.Builder
			fmtStr.WriteString("printf(\"{")
			vals := make([]string, len(ml.Items))
			for i, it := range ml.Items {
				if i > 0 {
					fmtStr.WriteString(",")
				}
				key := simpleMapKey(it.Key)
				if key == "" {
					return "", fmt.Errorf("json key unsupported")
				}
				key = strings.ReplaceAll(key, "\"", "\\\"")
				fmtStr.WriteString("\\\"" + key + "\\\":%d")
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				vals[i] = v
			}
			fmtStr.WriteString("}\\n\"")
			for _, v := range vals {
				fmtStr.WriteString(", ")
				fmtStr.WriteString(v)
			}
			fmtStr.WriteString(");")
			c.writeln(fmtStr.String())
			return "", nil
		}
		return "", fmt.Errorf("json unsupported")
	} else if call.Func == "values" {
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		if mp := mapLiteral(call.Args[0]); mp != nil {
			vals := make([]string, len(mp.Items))
			for i, it := range mp.Items {
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				vals[i] = v
			}
			c.lastType = "int[]"
			return fmt.Sprintf("(int[]){%s}", strings.Join(vals, ", ")), nil
		}
		if name := simpleIdent(call.Args[0]); name != "" {
			if vals, ok := c.mapVals[name]; ok {
				c.lastType = "int[]"
				return fmt.Sprintf("(int[]){%s}", strings.Join(vals, ", ")), nil
			}
		}
		return "", fmt.Errorf("values unsupported")
	}
	if t, ok := c.funRet[call.Func]; ok {
		c.lastType = t
	} else {
		c.lastType = ""
	}
	return fmt.Sprintf("%s(%s)", call.Func, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileLenExpr(e *parser.Expr) (string, error) {
	if lst := listLiteral(e); lst != nil {
		return fmt.Sprintf("%d", len(lst.Elems)), nil
	}
	if mp := mapLiteral(e); mp != nil {
		return fmt.Sprintf("%d", len(mp.Items)), nil
	}
	if name := simpleIdent(e); name != "" {
		if c.vars[name] == "int[]" {
			return fmt.Sprintf("sizeof(%s)/sizeof(%s[0])", name, name), nil
		}
		if c.vars[name] == "const char*" {
			return fmt.Sprintf("strlen(%s)", name), nil
		}
		if _, ok := c.mapTypes[name]; ok {
			if l, ok := c.lens[name]; ok {
				return fmt.Sprintf("%d", l), nil
			}
		}
	}
	if sliceLen, ok := sliceLength(e, c); ok {
		return sliceLen, nil
	}
	if c.exprIsString(e) {
		expr, err := c.compileExpr(e)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("strlen(%s)", expr), nil
	}
	return "", fmt.Errorf("len unsupported")
}

func sliceLength(e *parser.Expr, c *Compiler) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u.Value == nil || len(u.Value.Ops) != 1 {
		return "", false
	}
	op := u.Value.Ops[0]
	if op.Index == nil || op.Index.Colon == nil || op.Index.Start == nil || op.Index.End == nil {
		return "", false
	}
	startStr, err := c.compileExpr(op.Index.Start)
	if err != nil {
		return "", false
	}
	endStr, err := c.compileExpr(op.Index.End)
	if err != nil {
		return "", false
	}
	return fmt.Sprintf("(%s - %s)", endStr, startStr), true
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "1"
		}
		return "0"
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	default:
		return "0"
	}
}

func (c *Compiler) compileType(t *parser.TypeRef) (string, error) {
	if t == nil {
		return "int", nil
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int", "bool":
			return "int", nil
		case "string":
			return "const char*", nil
		default:
			if st, ok := c.env.GetStruct(*t.Simple); ok {
				_ = st // avoid unused
				return *t.Simple, nil
			}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			elem, _ := c.compileType(t.Generic.Args[0])
			return elem + "[]", nil
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			key, _ := c.compileType(t.Generic.Args[0])
			val, _ := c.compileType(t.Generic.Args[1])
			if key == "const char*" && val == "int" {
				return "EntrySI[]", nil
			}
			if key == "int" && val == "const char*" {
				return "EntryIS[]", nil
			}
		}
	}
	return "int", nil
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u.Value == nil || len(u.Ops) > 0 || len(u.Value.Ops) > 0 {
		return false
	}
	return u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil
}

func simpleIdent(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return ""
	}
	if u.Value.Target.Selector != nil && len(u.Value.Target.Selector.Tail) == 0 {
		return u.Value.Target.Selector.Root
	}
	return ""
}

func getQueryArg(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	return u.Value.Target.Query
}

func primaryIdent(p *parser.Primary) string {
	if p == nil || p.Selector == nil || len(p.Selector.Tail) > 0 {
		return ""
	}
	return p.Selector.Root
}

func collectIdentsExpr(e *parser.Expr, m map[string]bool) {
	if e == nil || e.Binary == nil {
		return
	}
	collectIdentsUnary(e.Binary.Left, m)
	for _, r := range e.Binary.Right {
		collectIdentsPostfix(r.Right, m)
	}
}

func collectIdentsUnary(u *parser.Unary, m map[string]bool) {
	if u == nil {
		return
	}
	collectIdentsPostfix(u.Value, m)
}

func collectIdentsPostfix(p *parser.PostfixExpr, m map[string]bool) {
	if p == nil {
		return
	}
	collectIdentsPrimary(p.Target, m)
	for _, op := range p.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				collectIdentsExpr(a, m)
			}
		}
		if op.Index != nil {
			if op.Index.Start != nil {
				collectIdentsExpr(op.Index.Start, m)
			}
			if op.Index.End != nil {
				collectIdentsExpr(op.Index.End, m)
			}
		}
	}
}

func collectIdentsPrimary(p *parser.Primary, m map[string]bool) {
	if p == nil {
		return
	}
	if p.Selector != nil && len(p.Selector.Tail) == 0 {
		m[p.Selector.Root] = true
	} else if p.List != nil {
		for _, e := range p.List.Elems {
			collectIdentsExpr(e, m)
		}
	} else if p.Map != nil {
		for _, item := range p.Map.Items {
			collectIdentsExpr(item.Value, m)
		}
	} else if p.If != nil {
		collectIdentsExpr(p.If.Cond, m)
		collectIdentsExpr(p.If.Then, m)
		if p.If.Else != nil {
			collectIdentsExpr(p.If.Else, m)
		}
	} else if p.Group != nil {
		collectIdentsExpr(p.Group, m)
	}
}

func collectIdentsStmts(sts []*parser.Statement, m map[string]bool) {
	for _, st := range sts {
		switch {
		case st.Return != nil:
			collectIdentsExpr(st.Return.Value, m)
		case st.Expr != nil:
			collectIdentsExpr(st.Expr.Expr, m)
		case st.Let != nil:
			collectIdentsExpr(st.Let.Value, m)
		case st.Assign != nil:
			collectIdentsExpr(st.Assign.Value, m)
		case st.If != nil:
			collectIdentsExpr(st.If.Cond, m)
			collectIdentsStmts(st.If.Then, m)
			if st.If.ElseIf != nil {
				tmp := &parser.IfStmt{Cond: st.If.ElseIf.Cond, Then: st.If.ElseIf.Then, Else: st.If.ElseIf.Else, ElseIf: st.If.ElseIf.ElseIf}
				collectIdentsExpr(tmp.Cond, m)
				collectIdentsStmts(tmp.Then, m)
			}
			collectIdentsStmts(st.If.Else, m)
		case st.While != nil:
			collectIdentsExpr(st.While.Cond, m)
			collectIdentsStmts(st.While.Body, m)
		case st.For != nil:
			collectIdentsExpr(st.For.Source, m)
			if st.For.RangeEnd != nil {
				collectIdentsExpr(st.For.RangeEnd, m)
			}
			collectIdentsStmts(st.For.Body, m)
		}
	}
}

func (c *Compiler) exprIsString(e *parser.Expr) bool {
	if name := simpleIdent(e); name != "" {
		if t, ok := c.vars[name]; ok && t == "const char*" {
			return true
		}
	}
	t := types.TypeOfExpr(e, c.env)
	return types.IsStringType(t)
}

func (c *Compiler) exprIsList(e *parser.Expr) bool {
	t := types.TypeOfExpr(e, c.env)
	return types.IsListType(t)
}

func (c *Compiler) unaryIsString(u *parser.Unary) bool {
	if len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 0 {
		if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
			if t, ok := c.vars[sel.Root]; ok && t == "const char*" {
				return true
			}
		}
	}
	t := types.TypeOfUnary(u, c.env)
	return types.IsStringType(t)
}

func (c *Compiler) postfixIsString(p *parser.PostfixExpr) bool {
	if id := primaryIdent(p.Target); id != "" {
		if t, ok := c.vars[id]; ok && t == "const char*" {
			return true
		}
	}
	t := types.TypeOfPostfix(p, c.env)
	return types.IsStringType(t)
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	name := fmt.Sprintf("lambda%d", c.lambdaCount)
	c.lambdaCount++

	// collect captured vars (simple identifiers)
	vars := make(map[string]bool)
	if fn.ExprBody != nil {
		collectIdentsExpr(fn.ExprBody, vars)
	}
	if fn.BlockBody != nil {
		collectIdentsStmts(fn.BlockBody, vars)
	}
	for _, p := range fn.Params {
		delete(vars, p.Name)
	}
	var capture string
	for v := range vars {
		if _, ok := c.vars[v]; ok {
			capture = v
			break
		}
	}

	retType, _ := c.compileType(fn.Return)
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		pt, _ := c.compileType(p.Type)
		params[i] = fmt.Sprintf("%s %s", pt, p.Name)
	}

	if capture == "" {
		fmt.Fprintf(&c.prelude, "typedef struct {} %s;\n", name)
	} else {
		capType := c.vars[capture]
		fmt.Fprintf(&c.prelude, "typedef struct { %s %s; } %s;\n", capType, capture, name)
	}
	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body
	c.indent++
	var oldAliases map[string]string
	if capture != "" {
		oldAliases = c.aliases
		c.aliases = map[string]string{capture: "__env->" + capture}
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			c.buf = oldBuf
			if capture != "" {
				c.aliases = oldAliases
			}
			return "", err
		}
		c.writeln(fmt.Sprintf("return %s;", body))
	} else {
		for _, st := range fn.BlockBody {
			if err := c.compileStmt(st); err != nil {
				c.buf = oldBuf
				if capture != "" {
					c.aliases = oldAliases
				}
				return "", err
			}
		}
	}
	if capture != "" {
		c.aliases = oldAliases
	}
	bodyOut := c.buf.Bytes()
	c.buf = oldBuf
	c.indent--
	fmt.Fprintf(&c.prelude, "static %s %s_apply(%s* __env, %s) {\n", retType, name, name, strings.Join(params, ", "))
	c.prelude.Write(bodyOut)
	c.prelude.WriteString("}\n\n")
	c.lastType = name
	if capture == "" {
		return fmt.Sprintf("(%s){ }", name), nil
	}
	return fmt.Sprintf("(%s){ .%s = %s }", name, capture, capture), nil
}

func isStringIndex(e *parser.Expr, c *Compiler) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u.Value == nil || len(u.Value.Ops) != 1 {
		return false
	}
	idx := u.Value.Ops[0]
	if idx.Index == nil || idx.Index.Start == nil || idx.Index.Colon != nil {
		return false
	}
	base := u.Value.Target
	if base.Lit != nil && base.Lit.Str != nil {
		return true
	}
	if base.Selector != nil {
		name := base.Selector.Root
		if c.vars[name] == "const char*" {
			return true
		}
	}
	return false
}

func listLiteral(e *parser.Expr) *parser.ListLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	return u.Value.Target.List
}

func mapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	return u.Value.Target.Map
}

func (c *Compiler) compileExistsQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil {
		return "", fmt.Errorf("unsupported query")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	length, err := c.compileLenExpr(q.Source)
	if err != nil {
		return "", err
	}
	cond := "true"
	if q.Where != nil {
		old := c.vars[q.Var]
		c.vars[q.Var] = "int"
		cond, err = c.compileExpr(q.Where)
		if old == "" {
			delete(c.vars, q.Var)
		} else {
			c.vars[q.Var] = old
		}
		if err != nil {
			return "", err
		}
	}
	name := fmt.Sprintf("exists_query_%d", c.lambdaCount)
	c.lambdaCount++
	fmt.Fprintf(&c.prelude, "static bool %s(const int* arr, int n) {\n", name)
	fmt.Fprintf(&c.prelude, "    for (int i = 0; i < n; i++) { int %s = arr[i]; if (%s) return true; }\n", q.Var, cond)
	fmt.Fprintf(&c.prelude, "    return false;\n}\n\n")
	c.lastType = "int"
	return fmt.Sprintf("%s(%s, %s)", name, src, length), nil
}

func (c *Compiler) compileInOp(left *parser.Unary, right *parser.PostfixExpr, leftStr, rightStr string) (string, error) {
	rType := types.TypeOfPostfix(right, c.env)
	if types.IsListType(rType) {
		var length string
		if right.Target.List != nil {
			length = fmt.Sprintf("%d", len(right.Target.List.Elems))
		} else if right.Target.Selector != nil {
			name := right.Target.Selector.Root
			if l, ok := c.lens[name]; ok {
				length = fmt.Sprintf("%d", l)
			} else {
				return "", fmt.Errorf("unknown length for %s", name)
			}
		} else {
			return "", fmt.Errorf("in unsupported")
		}
		c.needsContains = true
		return fmt.Sprintf("contains(%s, %s, %s)", rightStr, length, leftStr), nil
	}
	if types.IsStringType(rType) {
		if !c.postfixIsString(right) || !c.unaryIsString(left) {
			return "", fmt.Errorf("in type mismatch")
		}
		return fmt.Sprintf("(strstr(%s, %s) != NULL)", rightStr, leftStr), nil
	}
	if mt, ok := rType.(types.MapType); ok {
		var length string
		if right.Target.Map != nil {
			length = fmt.Sprintf("%d", len(right.Target.Map.Items))
		} else if right.Target.Selector != nil {
			name := right.Target.Selector.Root
			if l, ok := c.lens[name]; ok {
				length = fmt.Sprintf("%d", l)
			} else {
				return "", fmt.Errorf("unknown length for %s", name)
			}
		} else {
			return "", fmt.Errorf("in unsupported")
		}
		if _, ok := mt.Key.(types.StringType); ok {
			c.needsContainsSI = true
			return fmt.Sprintf("map_contains_str_int(%s, %s, %s)", rightStr, length, leftStr), nil
		}
		c.needsContainsIS = true
		return fmt.Sprintf("map_contains_int_str(%s, %s, %s)", rightStr, length, leftStr), nil
	}
	return "", fmt.Errorf("in unsupported")
}

func (c *Compiler) writeHelpers() {
	if c.needsNow {
		c.writeln("long long _now() {")
		c.indent++
		c.writeln("struct timespec ts;")
		c.writeln("clock_gettime(CLOCK_REALTIME, &ts);")
		c.writeln("return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsGetSI || c.needsContainsSI {
		c.writeln("typedef struct { const char* key; int value; } EntrySI;")
		c.writeln("")
	}
	if c.needsGetIS || c.needsContainsIS {
		c.writeln("typedef struct { int key; const char* value; } EntryIS;")
		c.writeln("")
	}
	if c.needsConcat {
		c.writeln("char* str_concat(const char* a, const char* b) {")
		c.indent++
		c.writeln("size_t len = strlen(a) + strlen(b);")
		c.writeln("char* res = malloc(len + 1);")
		c.writeln("strcpy(res, a);")
		c.writeln("strcat(res, b);")
		c.writeln("return res;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsToStr {
		c.writeln("char* to_str(int n) {")
		c.indent++
		c.writeln("char buf[32];")
		c.writeln("snprintf(buf, sizeof(buf), \"%d\", n);")
		c.writeln("return strdup(buf);")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSubstr {
		c.writeln("char* substr(const char* s, int start, int end) {")
		c.indent++
		c.writeln("int n = end - start;")
		c.writeln("char* out = malloc(n + 1);")
		c.writeln("strncpy(out, s + start, n);")
		c.writeln("out[n] = '\\0';")
		c.writeln("return out;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSliceList {
		c.writeln("int* list_slice(const int* arr, int start, int end) {")
		c.indent++
		c.writeln("int n = end - start;")
		c.writeln("int* out = malloc(sizeof(int) * n);")
		c.writeln("for (int i = 0; i < n; i++) out[i] = arr[start + i];")
		c.writeln("return out;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsContains {
		c.writeln("bool contains(const int* arr, int n, int val) {")
		c.indent++
		c.writeln("for (int i = 0; i < n; i++) {")
		c.indent++
		c.writeln("if (arr[i] == val) return true;")
		c.indent--
		c.writeln("}")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsPrintList {
		c.writeln("void print_list(const int* arr, int n) {")
		c.indent++
		c.writeln("for (int i = 0; i < n; i++) {")
		c.indent++
		c.writeln("if (i > 0) printf(\" \");")
		c.writeln("printf(\"%d\", arr[i]);")
		c.indent--
		c.writeln("}")
		c.writeln("printf(\"\\n\");")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsSum {
		c.writeln("int sum(const int* arr, int n) {")
		c.indent++
		c.writeln("int s = 0;")
		c.writeln("for (int i = 0; i < n; i++) s += arr[i];")
		c.writeln("return s;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsAvg {
		c.writeln("int avg(const int* arr, int n) {")
		c.indent++
		c.writeln("if (n == 0) return 0;")
		c.writeln("int s = 0;")
		c.writeln("for (int i = 0; i < n; i++) s += arr[i];")
		c.writeln("return s / n;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsMin {
		c.writeln("int min_val(const int* arr, int n) {")
		c.indent++
		c.writeln("int m = arr[0];")
		c.writeln("for (int i = 1; i < n; i++) if (arr[i] < m) m = arr[i];")
		c.writeln("return m;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsMax {
		c.writeln("int max_val(const int* arr, int n) {")
		c.indent++
		c.writeln("int m = arr[0];")
		c.writeln("for (int i = 1; i < n; i++) if (arr[i] > m) m = arr[i];")
		c.writeln("return m;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsGetSI {
		c.writeln("int map_get_str_int(const EntrySI* m, int n, const char* key) {")
		c.indent++
		c.writeln("for (int i = 0; i < n; i++) if (strcmp(m[i].key, key) == 0) return m[i].value;")
		c.writeln("return 0;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsGetIS {
		c.writeln("const char* map_get_int_str(const EntryIS* m, int n, int key) {")
		c.indent++
		c.writeln("for (int i = 0; i < n; i++) if (m[i].key == key) return m[i].value;")
		c.writeln("return \"\";")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsContainsSI {
		c.writeln("bool map_contains_str_int(const EntrySI* m, int n, const char* key) {")
		c.indent++
		c.writeln("for (int i = 0; i < n; i++) if (strcmp(m[i].key, key) == 0) return true;")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
	if c.needsContainsIS {
		c.writeln("bool map_contains_int_str(const EntryIS* m, int n, int key) {")
		c.indent++
		c.writeln("for (int i = 0; i < n; i++) if (m[i].key == key) return true;")
		c.writeln("return false;")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}
}
