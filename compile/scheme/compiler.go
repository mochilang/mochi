package schemecode

import (
	"bytes"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Scheme source code (minimal subset).
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
	inFun  bool
	vars   map[string]string // local variable types within a function
	tmp    int
}

// New creates a new Scheme compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, vars: map[string]string{}, tmp: 0}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile converts the given program to Scheme source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()

	// Function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Main body
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

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	prevVars := c.vars
	c.vars = map[string]string{}
	for _, p := range fn.Params {
		if p.Type != nil {
			if p.Type.Simple != nil && *p.Type.Simple == "string" {
				c.vars[p.Name] = "string"
			}
			if p.Type.Generic != nil && p.Type.Generic.Name == "map" {
				c.vars[p.Name] = "map"
			}
		}
	}
	c.writeln(fmt.Sprintf("(define (%s %s)", sanitizeName(fn.Name), strings.Join(params, " ")))
	c.indent++
	c.writeln("(call/cc (lambda (return)")
	c.indent++

	vars := map[string]bool{}
	collectVars(fn.Body, vars)
	names := make([]string, 0, len(vars))
	for v := range vars {
		names = append(names, sanitizeName(v))
	}
	sort.Strings(names)
	for _, n := range names {
		c.writeln(fmt.Sprintf("(define %s '())", n))
	}

	prev := c.inFun
	c.inFun = true
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.inFun = prev
	c.vars = prevVars
	c.indent--
	c.writeln("))")
	c.indent--
	c.writeln(")")
	return nil
}

func collectVars(stmts []*parser.Statement, vars map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.Let != nil:
			vars[s.Let.Name] = true
		case s.Var != nil:
			vars[s.Var.Name] = true
		case s.For != nil:
			collectVars(s.For.Body, vars)
		case s.If != nil:
			collectVars(s.If.Then, vars)
			if s.If.ElseIf != nil {
				collectVars([]*parser.Statement{{If: s.If.ElseIf}}, vars)
			}
			collectVars(s.If.Else, vars)
		case s.While != nil:
			collectVars(s.While.Body, vars)
		}
	}
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		expr, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		name := sanitizeName(s.Let.Name)
		if c.inFun {
			c.writeln(fmt.Sprintf("(set! %s %s)", name, expr))
		} else {
			c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
		}
		if s.Let.Type != nil {
			if s.Let.Type.Simple != nil && *s.Let.Type.Simple == "string" {
				c.vars[s.Let.Name] = "string"
			}
			if s.Let.Type.Generic != nil && s.Let.Type.Generic.Name == "map" {
				c.vars[s.Let.Name] = "map"
			}
		} else if s.Let.Value != nil && s.Let.Value.Binary != nil && s.Let.Value.Binary.Left != nil {
			if isStringLiteralUnary(s.Let.Value.Binary.Left) {
				c.vars[s.Let.Name] = "string"
			} else if isMapLiteralUnary(s.Let.Value.Binary.Left) {
				c.vars[s.Let.Name] = "map"
			}
		}
	case s.Var != nil:
		expr := "()"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			expr = v
		}
		name := sanitizeName(s.Var.Name)
		if c.inFun {
			c.writeln(fmt.Sprintf("(set! %s %s)", name, expr))
		} else {
			c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
		}
		if s.Var.Type != nil {
			if s.Var.Type.Simple != nil && *s.Var.Type.Simple == "string" {
				c.vars[s.Var.Name] = "string"
			}
			if s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "map" {
				c.vars[s.Var.Name] = "map"
			}
		} else if s.Var.Value != nil && s.Var.Value.Binary != nil && s.Var.Value.Binary.Left != nil {
			if isStringLiteralUnary(s.Var.Value.Binary.Left) {
				c.vars[s.Var.Name] = "string"
			} else if isMapLiteralUnary(s.Var.Value.Binary.Left) {
				c.vars[s.Var.Name] = "map"
			}
		}
	case s.Assign != nil:
		rhs, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		lhs := sanitizeName(s.Assign.Name)
		if len(s.Assign.Index) == 0 {
			c.writeln(fmt.Sprintf("(set! %s %s)", lhs, rhs))
			break
		}
		ie, err := c.compileExpr(s.Assign.Index[0].Start)
		if err != nil {
			return err
		}
		if c.vars[s.Assign.Name] == "string" {
			c.writeln(fmt.Sprintf("(string-set! %s %s %s)", lhs, ie, rhs))
		} else {
			c.writeln(fmt.Sprintf("(list-set! %s %s %s)", lhs, ie, rhs))
		}
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(return %s)", expr))
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileSimpleIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	default:
		// ignore unsupported statements
	}
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
		c.writeln(fmt.Sprintf("(let loop ((%s %s))", name, start))
		c.indent++
		c.writeln(fmt.Sprintf("(if (< %s %s)", name, end))
		c.indent++
		c.writeln("(begin")
		c.indent++
		prev := c.vars[st.Name]
		c.vars[st.Name] = ""
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		if prev != "" {
			c.vars[st.Name] = prev
		} else {
			delete(c.vars, st.Name)
		}
		c.writeln(fmt.Sprintf("(loop (+ %s 1))", name))
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln("'())")
		c.indent--
		c.writeln(")")
		return nil
	}

	src, err := c.compileExpr(st.Source)
	if err != nil {
		return err
	}
	t := c.inferType(st.Source)
	if _, ok := t.(types.StringType); ok {
		tmp := fmt.Sprintf("_str%d", c.tmp)
		c.tmp++
		c.writeln(fmt.Sprintf("(let ((%s %s))", tmp, src))
		c.indent++
		c.writeln("(let loop ((i 0))")
		c.indent++
		c.writeln(fmt.Sprintf("(if (< i (string-length %s))", tmp))
		c.indent++
		c.writeln("(begin")
		c.indent++
		c.writeln(fmt.Sprintf("(let ((%s (string-ref %s i)))", name, tmp))
		c.indent++
		prev := c.vars[st.Name]
		c.vars[st.Name] = "string"
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		if prev != "" {
			c.vars[st.Name] = prev
		} else {
			delete(c.vars, st.Name)
		}
		c.indent--
		c.writeln(")")
		c.writeln(fmt.Sprintf("(loop (+ i 1)))"))
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln("'())")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		return nil
	}

	// assume list
	tmp := fmt.Sprintf("_lst%d", c.tmp)
	c.tmp++
	c.writeln(fmt.Sprintf("(let ((%s %s))", tmp, src))
	c.indent++
	c.writeln(fmt.Sprintf("(let loop ((l %s))", tmp))
	c.indent++
	c.writeln("(if (pair? l)")
	c.indent++
	c.writeln("(begin")
	c.indent++
	c.writeln(fmt.Sprintf("(let ((%s (car l)))", name))
	c.indent++
	prev := c.vars[st.Name]
	c.vars[st.Name] = ""
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	if prev != "" {
		c.vars[st.Name] = prev
	} else {
		delete(c.vars, st.Name)
	}
	c.indent--
	c.writeln(")")
	c.writeln("(loop (cdr l))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln("'())")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileSimpleIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(if %s", cond))
	c.indent++
	c.writeln("(begin")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	if len(st.Else) > 0 {
		c.writeln("(begin")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	} else {
		c.writeln("'()")
	}
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("(let loop ()")
	c.indent++
	c.writeln(fmt.Sprintf("(if %s", cond))
	c.indent++
	c.writeln("(begin")
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.writeln("(loop)")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln("'())")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "()", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	leftAst := b.Left
	left, err := c.compileUnary(leftAst)
	if err != nil {
		return "", err
	}
	expr := left
	for _, op := range b.Right {
		rightAst := op.Right
		right, err := c.compilePostfix(rightAst)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+":
			if isListUnary(leftAst) || isListPostfix(rightAst) {
				expr = fmt.Sprintf("(append %s %s)", expr, right)
			} else if c.isStringUnary(leftAst) || c.isStringPostfix(rightAst) {
				expr = fmt.Sprintf("(string-append %s %s)", expr, right)
			} else {
				expr = fmt.Sprintf("(+ %s %s)", expr, right)
			}
		case "-", "*", "/":
			expr = fmt.Sprintf("(%s %s %s)", op.Op, expr, right)
		case "%":
			expr = fmt.Sprintf("(modulo %s %s)", expr, right)
		case "==":
			expr = fmt.Sprintf("(= %s %s)", expr, right)
		case "!=":
			expr = fmt.Sprintf("(not (= %s %s))", expr, right)
		case "&&":
			expr = fmt.Sprintf("(and %s %s)", expr, right)
		case "||":
			expr = fmt.Sprintf("(or %s %s)", expr, right)
		case "in":
			rightName := ""
			if rightAst != nil && rightAst.Target != nil && rightAst.Target.Selector != nil {
				rightName = rightAst.Target.Selector.Root
			}
			if c.vars[rightName] == "map" {
				expr = fmt.Sprintf("(if (assoc %s %s) #t #f)", left, right)
			} else {
				expr = fmt.Sprintf("(member %s %s)", left, right)
			}
		case "<", "<=", ">", ">=":
			expr = fmt.Sprintf("(%s %s %s)", op.Op, expr, right)
		default:
			expr = fmt.Sprintf("(%s %s %s)", op.Op, expr, right)
		}
		leftAst = &parser.Unary{Value: rightAst}
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			expr = fmt.Sprintf("(- %s)", expr)
		} else if op == "!" {
			expr = fmt.Sprintf("(not %s)", expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			targetName := ""
			if p.Target != nil && p.Target.Selector != nil {
				targetName = p.Target.Selector.Root
			}
			if c.vars[targetName] == "string" {
				expr = fmt.Sprintf("(string-ref %s %s)", expr, idx)
			} else if c.vars[targetName] == "map" {
				expr = fmt.Sprintf("(cdr (assoc %s %s))", idx, expr)
			} else {
				expr = fmt.Sprintf("(list-ref %s %s)", expr, idx)
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
			expr = fmt.Sprintf("(%s %s)", expr, strings.Join(args, " "))
			continue
		}
		if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				switch *op.Cast.Type.Simple {
				case "float":
					expr = fmt.Sprintf("(exact->inexact %s)", expr)
				case "int":
					expr = fmt.Sprintf("(inexact->exact %s)", expr)
				case "string":
					expr = fmt.Sprintf("(number->string %s)", expr)
				}
			}
			continue
		}
	}
	return expr, nil
}

func isListUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return isListPostfix(u.Value)
}

func isListPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return p.Target != nil && p.Target.List != nil
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return c.isStringPostfix(u.Value)
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.vars[p.Target.Selector.Root] == "string" {
			return true
		}
	}
	return isStringLiteralPrimary(p.Target)
}

func isStringLiteralUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return isStringLiteralPostfix(u.Value)
}

func isStringLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isStringLiteralPrimary(p.Target)
}

func isStringLiteralPrimary(p *parser.Primary) bool {
	return p != nil && p.Lit != nil && p.Lit.Str != nil
}

func isMapLiteralUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return isMapLiteralPostfix(u.Value)
}

func isMapLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return p.Target != nil && p.Target.Map != nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			s := strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64)
			if !strings.ContainsAny(s, ".eE") {
				s += ".0"
			}
			return s, nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return "#t", nil
			}
			return "#f", nil
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
		return "(list " + strings.Join(elems, " ") + ")", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("(cons %s %s)", k, v)
		}
		if len(items) == 0 {
			return "'()", nil
		}
		return "(list " + strings.Join(items, " ") + ")", nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return sanitizeName(p.Selector.Root), nil
		}
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.Call != nil:
		return c.compileCall(p.Call, "")
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
	switch call.Func {
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("(length %s)", args[0]), nil
	case "print":
		if len(args) == 0 {
			return "", fmt.Errorf("print expects at least 1 arg")
		}
		parts := make([]string, 0, len(args)*2+1)
		for i, a := range args {
			if i > 0 {
				parts = append(parts, "(display \" \")")
			}
			parts = append(parts, fmt.Sprintf("(display %s)", a))
		}
		parts = append(parts, "(newline)")
		return "(begin " + strings.Join(parts, " ") + ")", nil
	}
	if recv != "" {
		return fmt.Sprintf("(%s %s %s)", recv, call.Func, strings.Join(args, " ")), nil
	}
	return fmt.Sprintf("(%s %s)", sanitizeName(call.Func), strings.Join(args, " ")), nil
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
	return s
}

func (c *Compiler) inferType(e *parser.Expr) types.Type {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return types.AnyType{}
	}
	return c.inferUnaryType(e.Binary.Left)
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
	if u == nil {
		return types.AnyType{}
	}
	return c.inferPostfixType(u.Value)
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	if len(p.Ops) > 0 {
		return types.AnyType{}
	}
	return c.inferPrimaryType(p.Target)
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	switch {
	case p.Lit != nil:
		if p.Lit.Str != nil {
			return types.StringType{}
		}
		if p.Lit.Int != nil {
			return types.IntType{}
		}
	case p.List != nil:
		return types.ListType{Elem: types.AnyType{}}
	case p.Map != nil:
		return types.MapType{Key: types.AnyType{}, Value: types.AnyType{}}
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			return t
		}
	}
	return types.AnyType{}
}
