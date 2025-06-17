package fscode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into F# source code (subset used for LeetCode examples).
type Compiler struct {
	buf         bytes.Buffer
	indent      int
	env         *types.Env
	tmp         int
	currentFunc string
}

func New(env *types.Env) *Compiler { return &Compiler{env: env} }

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) newTmp() string {
	name := fmt.Sprintf("__tmp%d", c.tmp)
	c.tmp++
	return name
}

// Compile converts prog into F# source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("open System")
	c.writeln("")
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
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFunStmt(fn *parser.FunStmt) error {
	c.pushFunc(fn.Name)
	defer c.popFunc()
	exc := fmt.Sprintf("Return_%s", sanitizeName(fn.Name))
	ret := fsType(fn.Return)
	c.writeln(fmt.Sprintf("exception %s of %s", exc, ret))
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("(%s: %s)", sanitizeName(p.Name), fsType(p.Type))
	}
	c.writeln(fmt.Sprintf("let %s %s : %s =", sanitizeName(fn.Name), strings.Join(params, " "), ret))
	c.indent++
	c.writeln("try")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("failwith \"unreachable\"")
	c.indent--
	c.writeln(fmt.Sprintf("with %s v -> v", exc))
	c.indent--
	return nil
}

func (c *Compiler) compileTypeDecl(td *parser.TypeDecl) error {
	name := sanitizeName(td.Name)
	if len(td.Variants) > 0 {
		c.writeln(fmt.Sprintf("type %s =", name))
		c.indent++
		for _, v := range td.Variants {
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				fields[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), fsType(f.Type))
			}
			if len(fields) == 0 {
				c.writeln("| " + sanitizeName(v.Name))
			} else {
				c.writeln("| " + sanitizeName(v.Name) + " of " + strings.Join(fields, " * "))
			}
		}
		c.indent--
		return nil
	}
	// struct record
	c.writeln(fmt.Sprintf("type %s = {", name))
	c.indent++
	for i, m := range td.Members {
		if m.Field == nil {
			continue
		}
		sep := ";"
		if i == len(td.Members)-1 {
			sep = ""
		}
		c.writeln(fmt.Sprintf("%s: %s%s", sanitizeName(m.Field.Name), fsType(m.Field.Type), sep))
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		expr, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		name := sanitizeName(s.Let.Name)
		c.writeln(fmt.Sprintf("let %s = %s", name, expr))
	case s.Var != nil:
		expr, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		name := sanitizeName(s.Var.Name)
		c.writeln(fmt.Sprintf("let mutable %s = %s", name, expr))
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("raise (Return_%s (%s))", sanitizeName(c.currentFunc), expr))
	case s.Assign != nil:
		lhs := sanitizeName(s.Assign.Name)
		for _, idx := range s.Assign.Index {
			iexpr, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			lhs = fmt.Sprintf("%s.[%s]", lhs, iexpr)
		}
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s <- %s", lhs, val))
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	default:
		// ignore
	}
	return nil
}

var funcStack []string

func (c *Compiler) pushFunc(name string) {
	funcStack = append(funcStack, name)
	c.currentFunc = name
}

func (c *Compiler) popFunc() {
	if len(funcStack) > 0 {
		funcStack = funcStack[:len(funcStack)-1]
		if len(funcStack) > 0 {
			c.currentFunc = funcStack[len(funcStack)-1]
		} else {
			c.currentFunc = ""
		}
	}
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := sanitizeName(f.Name)
	useVar := name != "_"
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		loopVar := name
		if !useVar {
			loopVar = c.newTmp()
		}
		c.writeln(fmt.Sprintf("for %s = %s to %s - 1 do", loopVar, start, end))
		c.indent++
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		return nil
	}
	src, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	loopVar := name
	if !useVar {
		loopVar = c.newTmp()
	}
	c.writeln(fmt.Sprintf("for %s in %s do", loopVar, src))
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " do")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt) error {
	return c.compileIfChain(ifst, true)
}

func (c *Compiler) compileIfChain(ifst *parser.IfStmt, first bool) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	kw := "if"
	if !first {
		kw = "elif"
	}
	c.writeln(fmt.Sprintf("%s %s then", kw, cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if ifst.ElseIf != nil {
		return c.compileIfChain(ifst.ElseIf, false)
	}
	if len(ifst.Else) > 0 {
		c.writeln("else")
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	return nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var buf strings.Builder
	buf.WriteString("(match " + target + " with\n")
	for _, cs := range m.Cases {
		pat, err := c.compilePattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		buf.WriteString(" | " + pat + " -> " + res + "\n")
	}
	buf.WriteString(")")
	return buf.String(), nil
}

func (c *Compiler) compilePattern(e *parser.Expr) (string, error) {
	if isUnderscoreExpr(e) {
		return "_", nil
	}
	if call, ok := callPattern(e); ok {
		parts := make([]string, len(call.Args))
		for i, a := range call.Args {
			if id, ok := identName(a); ok {
				parts[i] = sanitizeName(id)
			} else {
				return "", fmt.Errorf("unsupported pattern")
			}
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), strings.Join(parts, ", ")), nil
	}
	if id, ok := identName(e); ok {
		return sanitizeName(id), nil
	}
	return "", fmt.Errorf("unsupported pattern")
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", fmt.Errorf("nil binary expr")
	}
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	leftList := c.isListUnary(b.Left)
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightList := c.isListPostfix(op.Right)
		symbol := op.Op
		switch op.Op {
		case "==":
			symbol = "="
		case "!=":
			symbol = "<>"
		case "+":
			if leftList && rightList {
				expr = fmt.Sprintf("Array.append %s %s", expr, right)
				leftList = true
				continue
			}
		}
		expr = fmt.Sprintf("(%s %s %s)", expr, symbol, right)
		leftList = false
	}
	return expr, nil
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
	for _, op := range p.Ops {
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			argStr := strings.Join(args, " ")
			if expr == "print" {
				if len(args) == 1 {
					return fmt.Sprintf("printfn \"%%A\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%A\" (%s)", argStr), nil
			}
			if expr == "len" {
				if len(args) != 1 {
					return "", fmt.Errorf("len expects 1 arg")
				}
				return fmt.Sprintf("%s.Length", args[0]), nil
			}
			expr = fmt.Sprintf("%s %s", expr, argStr)
			continue
		}
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			expr = fmt.Sprintf("%s.[%s]", expr, idx)
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
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
		return "[|" + strings.Join(elems, "; ") + "|]", nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		for _, s := range p.Selector.Tail {
			expr += "." + sanitizeName(s)
		}
		return expr, nil
	case p.Struct != nil:
		// struct literal or union variant
		if ut, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
			st := ut.Variants[p.Struct.Name]
			vals := make(map[string]string)
			for _, f := range p.Struct.Fields {
				v, err := c.compileExpr(f.Value)
				if err != nil {
					return "", err
				}
				vals[f.Name] = v
			}
			if len(st.Order) == 0 {
				return sanitizeName(p.Struct.Name), nil
			}
			parts := make([]string, len(st.Order))
			for i, n := range st.Order {
				parts[i] = vals[n]
			}
			return fmt.Sprintf("%s(%s)", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
		}
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s = %s", sanitizeName(f.Name), v)
		}
		return "{ " + strings.Join(parts, "; ") + " }", nil
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Group != nil:
		e, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + e + ")", nil
	}
	return "", fmt.Errorf("unsupported primary expression")
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	argStr := strings.Join(args, " ")
	switch call.Func {
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("(string %s)", args[0]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("%s.Length", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("%s.Length", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(Array.sum %s) / %s.Length", args[0], args[0]), nil
	case "print":
		if len(args) == 1 {
			return fmt.Sprintf("printfn \"%%A\" %s", args[0]), nil
		}
		return fmt.Sprintf("printfn \"%%A\" (%s)", argStr), nil
	default:
		return fmt.Sprintf("%s %s", sanitizeName(call.Func), argStr), nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	}
	return "", fmt.Errorf("unknown literal")
}

func fsType(t *parser.TypeRef) string {
	if t == nil {
		return "obj"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "float"
		case "bool":
			return "bool"
		case "string":
			return "string"
		case "void":
			return "unit"
		default:
			return sanitizeName(*t.Simple)
		}
	}
	if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
		return fsType(t.Generic.Args[0]) + "[]"
	}
	return "obj"
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
		s = "_" + s
	}
	return s
}

// --- helpers ---

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	return c.isListBinary(e.Binary)
}

func (c *Compiler) isListBinary(b *parser.BinaryExpr) bool {
	if b == nil {
		return false
	}
	if len(b.Right) == 0 {
		return c.isListUnary(b.Left)
	}
	// only handle simple a + b case
	if len(b.Right) == 1 && b.Right[0].Op == "+" {
		return c.isListUnary(b.Left) && c.isListPostfix(b.Right[0].Right)
	}
	return false
}

func (c *Compiler) isListUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isListPostfix(u.Value)
}

func (c *Compiler) isListPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	res := c.isListPrimary(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil || op.Call != nil {
			res = false
		}
		if op.Cast != nil {
			typ := op.Cast.Type
			if typ.Generic != nil && typ.Generic.Name == "list" && len(typ.Generic.Args) == 1 {
				res = true
			} else {
				res = false
			}
		}
	}
	return res
}

func (c *Compiler) isListPrimary(p *parser.Primary) bool {
	switch {
	case p == nil:
		return false
	case p.List != nil:
		return true
	case p.Group != nil:
		return c.isListExpr(p.Group)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		typ, err := c.env.GetVar(p.Selector.Root)
		if err == nil {
			if _, ok := typ.(types.ListType); ok {
				return true
			}
		}
		return false
	default:
		return false
	}
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	return p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
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
	return p.Target.Call, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}
