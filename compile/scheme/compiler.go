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
	buf           bytes.Buffer
	indent        int
	env           *types.Env
	inFun         bool
	vars          map[string]string // local variable types within a function
	needListSet   bool
	needStringSet bool
}

// New creates a new Scheme compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env, vars: map[string]string{}} }

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
	c.needListSet = false
	c.needStringSet = false
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
	code := c.buf.Bytes()
	if c.needListSet || c.needStringSet {
		var pre bytes.Buffer
		if c.needListSet {
			pre.WriteString("(define (list-set lst idx val)\n")
			pre.WriteString("    (let loop ((i idx) (l lst))\n")
			pre.WriteString("        (if (null? l)\n")
			pre.WriteString("            '()\n")
			pre.WriteString("            (if (= i 0)\n")
			pre.WriteString("                (cons val (cdr l))\n")
			pre.WriteString("                (cons (car l) (loop (- i 1) (cdr l))))))\n")
			pre.WriteString(")\n")
		}
		if c.needStringSet {
			pre.WriteString("(define (string-set str idx ch)\n")
			pre.WriteString("    (list->string (list-set (string->list str) idx ch))\n")
			pre.WriteString(")\n")
		}
		if pre.Len() > 0 {
			pre.WriteByte('\n')
		}
		pre.Write(code)
		code = pre.Bytes()
	}
	return code, nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	prevVars := c.vars
	c.vars = map[string]string{}
	for _, p := range fn.Params {
		if p.Type != nil && p.Type.Simple != nil && *p.Type.Simple == "string" {
			c.vars[p.Name] = "string"
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
		if (s.Let.Type != nil && s.Let.Type.Simple != nil && *s.Let.Type.Simple == "string") ||
			(s.Let.Type == nil && isStringExpr(s.Let.Value, c.vars)) {
			c.vars[s.Let.Name] = "string"
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
		if (s.Var.Type != nil && s.Var.Type.Simple != nil && *s.Var.Type.Simple == "string") ||
			(s.Var.Type == nil && isStringExpr(s.Var.Value, c.vars)) {
			c.vars[s.Var.Name] = "string"
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
		expr, err := c.compileIndexedSet(lhs, s.Assign.Index, rhs, c.vars[s.Assign.Name] == "string")
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(set! %s %s)", lhs, expr))
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

// compileIndexedSet builds a nested list/string update expression for an indexed assignment.
func (c *Compiler) compileIndexedSet(name string, idx []*parser.IndexOp, rhs string, isString bool) (string, error) {
	if len(idx) == 0 {
		return rhs, nil
	}
	ie, err := c.compileExpr(idx[0].Start)
	if err != nil {
		return "", err
	}
	if len(idx) == 1 {
		if isString {
			c.needStringSet = true
			return fmt.Sprintf("(string-set %s %s %s)", name, ie, rhs), nil
		}
		c.needListSet = true
		return fmt.Sprintf("(list-set %s %s %s)", name, ie, rhs), nil
	}
	inner, err := c.compileIndexedSet(fmt.Sprintf("(list-ref %s %s)", name, ie), idx[1:], rhs, false)
	if err != nil {
		return "", err
	}
	c.needListSet = true
	return fmt.Sprintf("(list-set %s %s %s)", name, ie, inner), nil
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
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
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
	root := rootNameExpr(st.Source)
	isStr := c.vars[root] == "string" || isStringExpr(st.Source, c.vars)
	idx := sanitizeName(name + "_idx")
	lenExpr := fmt.Sprintf("(length %s)", src)
	elemExpr := fmt.Sprintf("(list-ref %s %s)", src, idx)
	if isStr {
		lenExpr = fmt.Sprintf("(string-length %s)", src)
		elemExpr = fmt.Sprintf("(string-ref %s %s)", src, idx)
	}
	c.writeln(fmt.Sprintf("(let loop ((%s 0))", idx))
	c.indent++
	c.writeln(fmt.Sprintf("(if (< %s %s)", idx, lenExpr))
	c.indent++
	c.writeln("(begin")
	c.indent++
	if st.Name != "_" {
		c.writeln(fmt.Sprintf("(let ((%s %s))", name, elemExpr))
		c.indent++
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	} else {
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
	}
	c.writeln(fmt.Sprintf("(loop (+ %s 1))", idx))
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln("'())")
	c.indent--
	c.writeln(")")
	return nil
}

func rootNameExpr(e *parser.Expr) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return ""
	}
	return rootNameUnary(e.Binary.Left)
}

func rootNameUnary(u *parser.Unary) string {
	if u == nil {
		return ""
	}
	return rootNamePostfix(u.Value)
}

func rootNamePostfix(p *parser.PostfixExpr) string {
	if p == nil {
		return ""
	}
	if p.Target != nil && p.Target.Selector != nil {
		return p.Target.Selector.Root
	}
	return ""
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
			if c.vars[targetName] == "string" || isStringPrimary(p.Target) {
				expr = fmt.Sprintf("(string-ref %s %s)", expr, idx)
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
	return sanitizeName(p.Selector.Root), nil
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
		root := rootNameExpr(call.Args[0])
		if c.vars[root] == "string" || isStringExpr(call.Args[0], c.vars) {
			return fmt.Sprintf("(string-length %s)", args[0]), nil
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

// isStringExpr reports whether the expression resolves to a string literal or a
// variable tracked as a string in vars.
func isStringExpr(e *parser.Expr, vars map[string]string) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	if isStringUnary(e.Binary.Left, vars) {
		return true
	}
	return false
}

func isStringUnary(u *parser.Unary, vars map[string]string) bool {
	if u == nil {
		return false
	}
	if isStringPostfix(u.Value, vars) {
		return true
	}
	return false
}

func isStringPostfix(p *parser.PostfixExpr, vars map[string]string) bool {
	if p == nil {
		return false
	}
	if isStringPrimary(p.Target) {
		return true
	}
	if p.Target != nil && p.Target.Selector != nil {
		return vars[p.Target.Selector.Root] == "string"
	}
	return false
}

func isStringPrimary(p *parser.Primary) bool {
	return p != nil && p.Lit != nil && p.Lit.Str != nil
}
