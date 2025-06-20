package fscode

import (
	"bytes"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into F# source code (subset used for LeetCode examples).
type Compiler struct {
	buf         bytes.Buffer
	preamble    bytes.Buffer
	indent      int
	env         *types.Env
	tmp         int
	loopTmp     int
	funTmp      int
	currentFunc string
	loops       []loopCtx
	locals      map[string]bool
	localsStack []map[string]bool
	helpers     map[string]bool
	tests       []testInfo
}

type loopCtx struct {
	brk  string
	cont string
}

type testInfo struct {
	Func string
	Name string
}

func hasLoopCtrl(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		switch {
		case s.Break != nil || s.Continue != nil:
			return true
		case s.For != nil:
			if hasLoopCtrl(s.For.Body) {
				return true
			}
		case s.While != nil:
			if hasLoopCtrl(s.While.Body) {
				return true
			}
		case s.If != nil:
			if hasLoopCtrl(s.If.Then) || hasLoopCtrlIf(s.If.ElseIf) || hasLoopCtrl(s.If.Else) {
				return true
			}
		}
	}
	return false
}

func programHasLoopCtrl(stmts []*parser.Statement) bool {
	if hasLoopCtrl(stmts) {
		return true
	}
	for _, s := range stmts {
		if s.Fun != nil {
			if hasLoopCtrl(s.Fun.Body) {
				return true
			}
		}
	}
	return false
}

func hasLoopCtrlIf(ifst *parser.IfStmt) bool {
	if ifst == nil {
		return false
	}
	if hasLoopCtrl(ifst.Then) || hasLoopCtrl(ifst.Else) {
		return true
	}
	return hasLoopCtrlIf(ifst.ElseIf)
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, locals: make(map[string]bool), helpers: make(map[string]bool)}
}

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

func (c *Compiler) newLoopID() string {
	id := fmt.Sprintf("%d", c.loopTmp)
	c.loopTmp++
	return id
}

func (c *Compiler) newFunName() string {
	name := fmt.Sprintf("__anon%d", c.funTmp)
	c.funTmp++
	return name
}

// Compile converts prog into F# source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.preamble.Reset()
	c.tests = nil
	var header bytes.Buffer
	header.WriteString("open System\n")
	if programHasLoopCtrl(prog.Statements) {
		header.WriteString("exception BreakException of int\n")
		header.WriteString("exception ContinueException of int\n")
	}
	header.WriteByte('\n')
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
			continue
		}
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	if len(c.tests) > 0 {
		c.use("_run_test")
		c.writeln("let mutable failures = 0")
		for _, tinfo := range c.tests {
			c.writeln(fmt.Sprintf("if not (_run_test \"%s\" %s) then failures <- failures + 1", tinfo.Name, tinfo.Func))
		}
		c.writeln("if failures > 0 then")
		c.indent++
		c.writeln("printfn \"\\n[FAIL] %d test(s) failed.\" failures")
		c.indent--
	}
	c.emitRuntime()
	out := header.Bytes()
	if c.preamble.Len() > 0 {
		out = append(out, c.preamble.Bytes()...)
		out = append(out, '\n')
	}
	out = append(out, c.buf.Bytes()...)
	return out, nil
}

func (c *Compiler) compileFunStmt(fn *parser.FunStmt) error {
	c.env.SetFunc(fn.Name, fn)
	nested := len(funcStack) > 0
	c.pushFunc(fn.Name)
	defer c.popFunc()
	exc := fmt.Sprintf("Return_%s", sanitizeName(fn.Name))
	ret := fsType(fn.Return)
	excType := ret
	if strings.ContainsAny(ret, " ->") {
		excType = "(" + ret + ")"
	}
	line := fmt.Sprintf("exception %s of %s", exc, excType)
	if nested {
		c.preamble.WriteString(line + "\n")
	} else {
		c.writeln(line)
	}
	params := make([]string, len(fn.Params))
	paramTypes := make([]types.Type, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("(%s: %s)", sanitizeName(p.Name), fsType(p.Type))
		paramTypes[i] = c.resolveTypeRef(p.Type)
	}
	kw := "let rec"
	c.writeln(fmt.Sprintf("%s %s %s : %s =", kw, sanitizeName(fn.Name), strings.Join(params, " "), ret))

	child := types.NewEnv(c.env)
	for i, p := range fn.Params {
		child.SetVar(p.Name, paramTypes[i], true)
	}
	origEnv := c.env
	c.env = child

	c.indent++
	c.writeln("try")
	c.indent++
	for _, p := range fn.Params {
		name := sanitizeName(p.Name)
		c.writeln(fmt.Sprintf("let mutable %s = %s", name, name))
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			c.env = origEnv
			return err
		}
	}
	c.writeln("failwith \"unreachable\"")
	c.indent--
	c.writeln(fmt.Sprintf("with %s v -> v", exc))
	c.indent--
	c.env = origEnv
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln(fmt.Sprintf("let %s() =", name))
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.tests = append(c.tests, testInfo{Func: name, Name: t.Name})
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
		if c.isMapExpr(s.Let.Value) {
			c.locals[name] = true
		}
		if c.env != nil {
			typ := c.resolveTypeRef(s.Let.Type)
			if s.Let.Type == nil {
				typ = c.inferExprType(s.Let.Value)
			}
			c.env.SetVar(s.Let.Name, typ, false)
		}
	case s.Var != nil:
		expr, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		name := sanitizeName(s.Var.Name)
		c.writeln(fmt.Sprintf("let mutable %s = %s", name, expr))
		if c.isMapExpr(s.Var.Value) {
			c.locals[name] = true
		}
		if c.env != nil {
			typ := c.resolveTypeRef(s.Var.Type)
			if s.Var.Type == nil {
				typ = c.inferExprType(s.Var.Value)
			}
			c.env.SetVar(s.Var.Name, typ, true)
		}
	case s.Fun != nil:
		return c.compileFunStmt(s.Fun)
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("raise (Return_%s (%s))", sanitizeName(c.currentFunc), expr))
	case s.Assign != nil:
		base := sanitizeName(s.Assign.Name)
		idxParts := []string{}
		for _, idx := range s.Assign.Index {
			iexpr, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			idxParts = append(idxParts, iexpr)
		}
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		if c.locals[base] && len(idxParts) == 1 {
			c.writeln(fmt.Sprintf("%s <- Map.add %s %s %s", base, idxParts[0], val, base))
		} else {
			lhs := base
			for _, p := range idxParts {
				lhs = fmt.Sprintf("%s.[%s]", lhs, p)
			}
			c.writeln(fmt.Sprintf("%s <- %s", lhs, val))
		}
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("ignore (%s)", expr))
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		if len(c.loops) == 0 {
			return fmt.Errorf("break not in loop")
		}
		c.writeln(fmt.Sprintf("raise (%s)", c.loops[len(c.loops)-1].brk))
	case s.Continue != nil:
		if len(c.loops) == 0 {
			return fmt.Errorf("continue not in loop")
		}
		c.writeln(fmt.Sprintf("raise (%s)", c.loops[len(c.loops)-1].cont))
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	default:
		// ignore
	}
	return nil
}

var funcStack []string

func (c *Compiler) pushFunc(name string) {
	funcStack = append(funcStack, name)
	c.currentFunc = name
	c.localsStack = append(c.localsStack, c.locals)
	newLocals := make(map[string]bool)
	for k, v := range c.locals {
		newLocals[k] = v
	}
	c.locals = newLocals
}

func (c *Compiler) pushLoop(brk, cont string) {
	c.loops = append(c.loops, loopCtx{brk: brk, cont: cont})
}

func (c *Compiler) popLoop() {
	if len(c.loops) > 0 {
		c.loops = c.loops[:len(c.loops)-1]
	}
}

func (c *Compiler) popFunc() {
	if len(funcStack) > 0 {
		funcStack = funcStack[:len(funcStack)-1]
		if len(c.localsStack) > 0 {
			c.locals = c.localsStack[len(c.localsStack)-1]
			c.localsStack = c.localsStack[:len(c.localsStack)-1]
		}
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
	if useVar && c.env != nil {
		var typ types.Type = types.AnyType{}
		if f.RangeEnd != nil {
			typ = types.IntType{}
		} else if c.isStringExpr(f.Source) {
			typ = types.StringType{}
		} else {
			if lt, ok := c.inferExprType(f.Source).(types.ListType); ok {
				typ = lt.Elem
			}
		}
		c.env.SetVar(f.Name, typ, true)
	}
	ctrl := hasLoopCtrl(f.Body)
	var brk, cont string
	var id string
	if ctrl {
		id = c.newLoopID()
		brk = fmt.Sprintf("BreakException %s", id)
		cont = fmt.Sprintf("ContinueException %s", id)
		c.writeln("try")
		c.indent++
	}
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
		if ctrl {
			c.pushLoop(brk, cont)
			c.writeln("try")
			c.indent++
		}
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		if ctrl {
			c.indent--
			c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
			c.popLoop()
		}
		c.indent--
		if ctrl {
			c.indent--
			c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
		}
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
	if c.isStringExpr(f.Source) && useVar {
		c.writeln(fmt.Sprintf("let %s = string %s", loopVar, loopVar))
	}
	if ctrl {
		c.pushLoop(brk, cont)
		c.writeln("try")
		c.indent++
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
		c.popLoop()
	}
	c.indent--
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
	}
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	ctrl := hasLoopCtrl(w.Body)
	var brk, cont string
	var id string
	if ctrl {
		id = c.newLoopID()
		brk = fmt.Sprintf("BreakException %s", id)
		cont = fmt.Sprintf("ContinueException %s", id)
		c.writeln("try")
		c.indent++
	}
	c.writeln("while " + cond + " do")
	c.indent++
	if ctrl {
		c.pushLoop(brk, cont)
		c.writeln("try")
		c.indent++
	}
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
		c.popLoop()
	}
	c.indent--
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
	}
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
	buf.WriteString("(match " + target + " with")
	for _, cs := range m.Cases {
		pat, err := c.compilePattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		buf.WriteString(" | " + pat + " -> " + res)
	}
	buf.WriteString(")")
	return buf.String(), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	orig := c.env
	child := types.NewEnv(c.env)
	if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
		child.SetVar(q.Var, lt.Elem, true)
	} else {
		child.SetVar(q.Var, types.AnyType{}, true)
	}
	fromSrc := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrc[i] = fs
		if lt, ok := c.inferExprType(f.Src).(types.ListType); ok {
			child.SetVar(f.Var, lt.Elem, true)
		} else {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
	}
	c.env = child
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var sortExpr string
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	c.env = orig

	var buf strings.Builder
	buf.WriteString("[|")
	buf.WriteString(fmt.Sprintf(" for %s in %s do ", sanitizeName(q.Var), src))
	for i, f := range q.Froms {
		buf.WriteString(fmt.Sprintf("for %s in %s do ", sanitizeName(f.Var), fromSrc[i]))
	}
	if q.Sort != nil {
		buf.WriteString(fmt.Sprintf("yield (%s, %s) |]", sortExpr, sel))
		buf.WriteString(fmt.Sprintf(" |> Array.sortBy fst |> Array.map snd"))
	} else {
		buf.WriteString(fmt.Sprintf("yield %s |]", sel))
	}
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
	if lit := extractLiteral(e); lit != nil {
		return c.compileLiteral(lit)
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

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}

	operands := []string{first}
	lists := []bool{c.isListUnary(b.Left)}
	maps := []bool{c.isMapUnary(b.Left)}
	strs := []bool{c.isStringUnary(b.Left)}
	ops := []string{}

	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		lists = append(lists, c.isListPostfix(part.Right))
		maps = append(maps, c.isMapPostfix(part.Right))
		strs = append(strs, c.isStringPostfix(part.Right))
		ops = append(ops, part.Op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "except", "intersect"},
	}

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if contains(lvl, ops[i]) {
				left := operands[i]
				right := operands[i+1]
				op := ops[i]

				symbol := op
				switch op {
				case "==":
					symbol = "="
				case "!=":
					symbol = "<>"
				}

				if op == "+" && (lists[i] || lists[i+1]) {
					operands[i] = fmt.Sprintf("Array.append %s %s", left, right)
					lists[i] = true
					maps[i] = false
					strs[i] = false
				} else if op == "+" && (strs[i] || strs[i+1]) {
					if !strs[i] {
						left = fmt.Sprintf("(string %s)", left)
					}
					if !strs[i+1] {
						right = fmt.Sprintf("(string %s)", right)
					}
					operands[i] = fmt.Sprintf("(%s + %s)", left, right)
					strs[i] = true
					lists[i] = false
					maps[i] = false
				} else if op == "in" {
					if maps[i+1] {
						operands[i] = fmt.Sprintf("Map.containsKey %s %s", left, right)
					} else {
						operands[i] = fmt.Sprintf("Array.contains %s %s", left, right)
					}
					lists[i] = false
					maps[i] = false
					strs[i] = false
				} else {
					operands[i] = fmt.Sprintf("(%s %s %s)", left, symbol, right)
					lists[i] = false
					maps[i] = false
					strs[i] = false
				}

				operands = append(operands[:i+1], operands[i+2:]...)
				lists = append(lists[:i+1], lists[i+2:]...)
				maps = append(maps[:i+1], maps[i+2:]...)
				strs = append(strs[:i+1], strs[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
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
		op := u.Ops[i]
		if op == "!" {
			val = fmt.Sprintf("(not %s)", val)
		} else {
			val = fmt.Sprintf("(%s%s)", op, val)
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	isStr := c.isStringPrimary(p.Target)
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
				if c.isMapExpr(op.Call.Args[0]) {
					return fmt.Sprintf("Map.count (%s)", args[0]), nil
				}
				return fmt.Sprintf("%s.Length", args[0]), nil
			}
			expr = fmt.Sprintf("%s %s", expr, argStr)
			isStr = false
			continue
		}
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					start, err = c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
				}
				end := fmt.Sprintf("%s.Length", expr)
				if op.Index.End != nil {
					end, err = c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
				}
				expr = fmt.Sprintf("%s.[%s .. (%s - 1)]", expr, start, end)
			} else {
				if isStr {
					if v, ok := intLiteral(op.Index.Start); ok && v >= 0 {
						expr = fmt.Sprintf("(string %s.[%d])", expr, v)
					} else {
						idxExpr := fmt.Sprintf("(if %s < 0 then %s.Length + %s else %s)", idx, expr, idx, idx)
						expr = fmt.Sprintf("(string %s.[%s])", expr, idxExpr)
					}
					isStr = true
				} else {
					expr = fmt.Sprintf("%s.[%s]", expr, idx)
					isStr = false
				}
			}
			continue
		}
		if op.Cast != nil {
			typ := op.Cast.Type
			if typ.Simple != nil {
				switch *typ.Simple {
				case "int":
					expr = fmt.Sprintf("(int %s)", expr)
				case "float":
					expr = fmt.Sprintf("(float %s)", expr)
				case "bool":
					expr = fmt.Sprintf("(bool %s)", expr)
				case "string":
					expr = fmt.Sprintf("(string %s)", expr)
				default:
					expr = fmt.Sprintf("(%s : %s)", expr, fsType(typ))
				}
			} else {
				expr = fmt.Sprintf("(%s : %s)", expr, fsType(typ))
			}
			isStr = typ.Simple != nil && *typ.Simple == "string"
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
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
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
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
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
		if c.isMapExpr(call.Args[0]) {
			return fmt.Sprintf("Map.count (%s)", args[0]), nil
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
		return fmt.Sprintf("((Array.sum %s) / %s.Length)", args[0], args[0]), nil
	case "print":
		if len(args) == 1 {
			return fmt.Sprintf("printfn \"%%A\" (%s)", args[0]), nil
		}
		return fmt.Sprintf("printfn \"%%A\" (%s)", strings.Join(args, ", ")), nil
	default:
		for i, a := range args {
			if !isSimpleIdent(a) {
				args[i] = "(" + a + ")"
			}
		}
		return fmt.Sprintf("%s %s", sanitizeName(call.Func), strings.Join(args, " ")), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = fmt.Sprintf("(%s: %s)", sanitizeName(p.Name), fsType(p.Type))
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(fun %s -> %s)", strings.Join(params, " "), expr), nil
	}
	name := c.newFunName()
	sub := New(c.env)
	sub.tmp = c.tmp
	sub.loopTmp = c.loopTmp
	sub.funTmp = c.funTmp
	fs := &parser.FunStmt{Name: name, Params: fn.Params, Return: fn.Return, Body: fn.BlockBody}
	if err := sub.compileFunStmt(fs); err != nil {
		return "", err
	}
	c.tmp = sub.tmp
	c.loopTmp = sub.loopTmp
	c.funTmp = sub.funTmp
	if sub.preamble.Len() > 0 {
		c.preamble.Write(sub.preamble.Bytes())
	}
	c.preamble.Write(sub.buf.Bytes())
	c.preamble.WriteByte('\n')
	return name, nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "None"
	if l.Path != nil {
		path = fmt.Sprintf("Some %q", *l.Path)
	}
	opts := "None"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("Some (%s)", v)
	}
	c.use("_load")
	return fmt.Sprintf("_load %s %s", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "None"
	if s.Path != nil {
		path = fmt.Sprintf("Some %q", *s.Path)
	}
	opts := "None"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = fmt.Sprintf("Some (%s)", v)
	}
	c.use("_save")
	return fmt.Sprintf("_save %s %s %s", src, path, opts), nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if not (%s) then failwith \"expect failed\"", expr))
	return nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		k, err := c.compileExpr(it.Key)
		if err != nil {
			return "", err
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("(%s, %s)", k, v)
	}
	return "Map.ofList [" + strings.Join(items, "; ") + "]", nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		s := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			if !strings.Contains(s, ".") {
				s += ".0"
			}
		}
		return s, nil
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
	if t.Fun != nil {
		ret := "unit"
		if t.Fun.Return != nil {
			ret = fsType(t.Fun.Return)
		}
		for i := len(t.Fun.Params) - 1; i >= 0; i-- {
			ret = fmt.Sprintf("%s -> %s", fsType(t.Fun.Params[i]), ret)
		}
		return ret
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
	switch s {
	case "abstract", "and", "as", "assert", "base", "begin", "class", "default",
		"delegate", "do", "done", "downcast", "downto", "elif", "else", "end",
		"exception", "extern", "false", "finally", "for", "fun", "function",
		"if", "in", "inherit", "inline", "interface", "internal", "lazy", "let",
		"match", "member", "module", "mutable", "namespace", "new", "null", "of",
		"open", "or", "override", "private", "public", "rec", "return", "sig",
		"static", "struct", "then", "to", "true", "try", "type", "upcast", "use",
		"val", "void", "when", "while", "with", "yield":
		s = "_" + s
	}
	return s
}

func isSimpleIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		if !(r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z')) {
			return false
		}
	}
	return true
}

// --- helpers ---

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.ListType)
	return ok
}

func (c *Compiler) isListBinary(b *parser.BinaryExpr) bool {
	_, ok := c.inferBinaryType(b).(types.ListType)
	return ok
}

func (c *Compiler) isListUnary(u *parser.Unary) bool {
	_, ok := c.inferUnaryType(u).(types.ListType)
	return ok
}

func (c *Compiler) isListPostfix(p *parser.PostfixExpr) bool {
	_, ok := c.inferPostfixType(p).(types.ListType)
	return ok
}

func (c *Compiler) isListPrimary(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.ListType)
	return ok
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

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.StringType)
	return ok
}

func (c *Compiler) isStringBinary(b *parser.BinaryExpr) bool {
	_, ok := c.inferBinaryType(b).(types.StringType)
	return ok
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	_, ok := c.inferUnaryType(u).(types.StringType)
	return ok
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	_, ok := c.inferPostfixType(p).(types.StringType)
	return ok
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.StringType)
	return ok
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.MapType)
	return ok
}

func (c *Compiler) isMapBinary(b *parser.BinaryExpr) bool {
	_, ok := c.inferBinaryType(b).(types.MapType)
	return ok
}

func (c *Compiler) isMapUnary(u *parser.Unary) bool {
	_, ok := c.inferUnaryType(u).(types.MapType)
	return ok
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	_, ok := c.inferPostfixType(p).(types.MapType)
	return ok
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.MapType)
	return ok
}

func intLiteral(e *parser.Expr) (int, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return 0, false
	}
	u := e.Binary.Left
	negate := false
	if len(u.Ops) > 0 {
		if len(u.Ops) == 1 && u.Ops[0] == "-" {
			negate = true
		} else {
			return 0, false
		}
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Lit == nil || p.Target.Lit.Int == nil {
		return 0, false
	}
	v := *p.Target.Lit.Int
	if negate {
		v = -v
	}
	return v, true
}

func extractLiteral(e *parser.Expr) *parser.Literal {
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
	if p.Target != nil {
		return p.Target.Lit
	}
	return nil
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitRuntime() {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		for _, line := range strings.Split(helperMap[n], "\n") {
			if line == "" {
				c.writeln("")
			} else {
				c.writeln(line)
			}
		}
	}
}
