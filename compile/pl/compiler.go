package plcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	tmp     int
	vars    map[string]string
	currFun string
	helpers map[string]bool
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env, vars: make(map[string]string), helpers: make(map[string]bool)}
}

// Compile translates a Mochi AST into Prolog source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()

	var body bytes.Buffer
	origBuf := c.buf
	c.buf = body

	// function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	tmpBuf := c.buf
	tmpIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = tmpIndent + 1
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s, "_"); err != nil {
			c.buf = origBuf
			return nil, err
		}
	}
	b := c.buf.Bytes()
	if bytes.HasSuffix(b, []byte(",\n")) {
		b = b[:len(b)-2]
		b = append(b, '\n')
	}
	c.buf = tmpBuf
	if len(b) == 0 {
		c.writeln("main :- true.")
	} else {
		c.writeln("main :-")
		c.buf.Write(b)
		c.writeln(".")
	}
	c.indent = tmpIndent
	c.writeln(":- initialization(main, main).")

	bodyBytes := append([]byte(nil), c.buf.Bytes()...)
	c.buf = origBuf
	c.indent = 0
	c.writeln(":- style_check(-singleton).")
	c.emitHelpers()
	c.buf.Write(bodyBytes)

	return c.buf.Bytes(), nil
}

func (c *Compiler) newVar() string {
	v := fmt.Sprintf("_V%d", c.tmp)
	c.tmp++
	return v
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitHelpers() {
	if c.helpers["slice"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperSlice, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["tolist"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperToList, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["getitem"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperGetItem, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["setitem"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperSetItem, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
	if c.helpers["contains"] {
		for _, line := range strings.Split(strings.TrimSuffix(helperContains, "\n"), "\n") {
			c.writeln(line)
		}
		c.writeln("")
	}
}

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
	c.writeln(fmt.Sprintf("%s(%s, %s) :-", name, strings.Join(params, ", "), ret))
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
	if fallback != nil {
		val, err := c.compileExpr(fallback.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s(%s, %s) :-", name, strings.Join(params, ", "), ret))
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
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("%s = %s,", name, val.val))
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
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
	case s.Expr != nil:
		if call := s.Expr.Expr.Binary.Left.Value.Target.Call; call != nil && call.Func == "print" {
			if len(call.Args) != 1 {
				return fmt.Errorf("print expects 1 arg")
			}
			arg, err := c.compileExpr(call.Args[0])
			if err != nil {
				return err
			}
			for _, line := range arg.code {
				c.writeln(line)
			}
			c.writeln(fmt.Sprintf("writeln(%s),", arg.val))
		} else {
			return fmt.Errorf("unsupported expression statement")
		}
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.For != nil:
		return c.compileFor(s.For, ret)
	case s.Break != nil:
		c.writeln("throw(break)")
		return nil
	case s.If != nil:
		return c.compileIf(s.If, ret, true)
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
		c.use("tolist")
		c.writeln(fmt.Sprintf("to_list(%s, %s),", src.val, listVar))
		loopVar := sanitizeVar(f.Name)
		c.writeln(fmt.Sprintf("forall(member(%s, %s), (", loopVar, listVar))
		c.indent++
		for _, s := range f.Body {
			if err := c.compileStmt(s, ret); err != nil {
				return err
			}
		}
		c.writeln("true")
		c.indent--
		c.writeln(")),")
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
	c.writeln(fmt.Sprintf("forall(between(%s, %s, %s), (", start.val, tempEnd, loopVar))
	c.indent++
	for _, s := range f.Body {
		if err := c.compileStmt(s, ret); err != nil {
			return err
		}
	}
	c.writeln("true")
	c.indent--
	c.writeln(")),")
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
	name := sanitizeAtom(c.currFun + "_" + v.Name)
	c.vars[v.Name] = name
	if v.Value != nil {
		val, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", name, val.val))
	} else {
		c.writeln(fmt.Sprintf("nb_setval(%s, 0),", name))
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	name, ok := c.vars[a.Name]
	if !ok {
		return fmt.Errorf("unknown variable %s", a.Name)
	}
	if len(a.Index) == 0 {
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return err
		}
		for _, line := range val.code {
			c.writeln(line)
		}
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", name, val.val))
		return nil
	}
	if len(a.Index) >= 1 && allSimple(a.Index) {
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
	for _, s := range stmt.Body {
		if err := c.compileStmt(s, ret); err != nil {
			return err
		}
	}
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

func isListExpr(u *parser.Unary) bool {
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

// --- Expressions ---

type exprRes struct {
	code []string
	val  string
}

func (c *Compiler) compileExpr(e *parser.Expr) (exprRes, error) {
	if e == nil {
		return exprRes{val: ""}, nil
	}
	return c.compileBinary(e.Binary)
}

type operand struct {
	expr   exprRes
	isList bool
}

func contains[T comparable](sl []T, t T) bool {
	for _, v := range sl {
		if v == t {
			return true
		}
	}
	return false
}

func (c *Compiler) binaryOp(left operand, op string, right operand) (operand, error) {
	res := exprRes{}
	res.code = append(res.code, left.expr.code...)
	res.code = append(res.code, right.expr.code...)
	switch op {
	case "+":
		if left.isList || right.isList {
			tmp := c.newVar()
			res.code = append(res.code, fmt.Sprintf("append(%s, %s, %s)", left.expr.val, right.expr.val, tmp)+",")
			res.val = tmp
			return operand{expr: res, isList: true}, nil
		}
		tmp := c.newVar()
		res.code = append(res.code, fmt.Sprintf("%s is %s + %s,", tmp, left.expr.val, right.expr.val))
		res.val = tmp
		return operand{expr: res}, nil
	case "-":
		tmp := c.newVar()
		res.code = append(res.code, fmt.Sprintf("%s is %s - %s,", tmp, left.expr.val, right.expr.val))
		res.val = tmp
		return operand{expr: res}, nil
	case "*":
		tmp := c.newVar()
		res.code = append(res.code, fmt.Sprintf("%s is %s * %s,", tmp, left.expr.val, right.expr.val))
		res.val = tmp
		return operand{expr: res}, nil
	case "/":
		tmp := c.newVar()
		res.code = append(res.code, fmt.Sprintf("%s is %s // %s,", tmp, left.expr.val, right.expr.val))
		res.val = tmp
		return operand{expr: res}, nil
	case "%":
		tmp := c.newVar()
		res.code = append(res.code, fmt.Sprintf("%s is %s mod %s,", tmp, left.expr.val, right.expr.val))
		res.val = tmp
		return operand{expr: res}, nil
	case "==":
		res.val = fmt.Sprintf("%s =:= %s", left.expr.val, right.expr.val)
		return operand{expr: res}, nil
	case "!=":
		res.val = fmt.Sprintf("%s =\\= %s", left.expr.val, right.expr.val)
		return operand{expr: res}, nil
	case "<":
		res.val = fmt.Sprintf("%s < %s", left.expr.val, right.expr.val)
		return operand{expr: res}, nil
	case "<=":
		res.val = fmt.Sprintf("%s =< %s", left.expr.val, right.expr.val)
		return operand{expr: res}, nil
	case ">":
		res.val = fmt.Sprintf("%s > %s", left.expr.val, right.expr.val)
		return operand{expr: res}, nil
	case ">=":
		res.val = fmt.Sprintf("%s >= %s", left.expr.val, right.expr.val)
		return operand{expr: res}, nil
	case "&&":
		res.val = fmt.Sprintf("(%s, %s)", left.expr.val, right.expr.val)
		return operand{expr: res}, nil
	case "||":
		res.val = fmt.Sprintf("(%s ; %s)", left.expr.val, right.expr.val)
		return operand{expr: res}, nil
	case "in":
		tmp := c.newVar()
		c.use("contains")
		res.code = append(res.code, fmt.Sprintf("contains(%s, %s, %s),", right.expr.val, left.expr.val, tmp))
		res.val = tmp
		return operand{expr: res}, nil
	default:
		return operand{}, fmt.Errorf("unsupported operator %s", op)
	}
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (exprRes, error) {
	first, err := c.compileUnary(b.Left)
	if err != nil {
		return exprRes{}, err
	}
	operands := []operand{{expr: first, isList: isListExpr(b.Left)}}
	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return exprRes{}, err
		}
		operands = append(operands, operand{expr: r, isList: isListPostfix(part.Right)})
		ops = append(ops, part.Op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
	}

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if !contains(lvl, ops[i]) {
				i++
				continue
			}
			res, err := c.binaryOp(operands[i], ops[i], operands[i+1])
			if err != nil {
				return exprRes{}, err
			}
			operands[i] = res
			operands = append(operands[:i+1], operands[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return exprRes{}, fmt.Errorf("unexpected state after binary compilation")
	}
	return operands[0].expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (exprRes, error) {
	res, err := c.compilePostfix(u.Value)
	if err != nil {
		return exprRes{}, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			tmp := c.newVar()
			res.code = append(res.code, fmt.Sprintf("%s is -(%s),", tmp, res.val))
			res.val = tmp
		case "!":
			tmp := c.newVar()
			res.code = append(res.code, fmt.Sprintf("(\\+ %s -> %s = true ; %s = false),", res.val, tmp, tmp))
			res.val = tmp
		default:
			return exprRes{}, fmt.Errorf("unsupported unary op")
		}
	}
	return res, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (exprRes, error) {
	res, err := c.compilePrimary(p.Target)
	if err != nil {
		return exprRes{}, err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon != nil {
				start, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return exprRes{}, err
				}
				end, err := c.compileExpr(op.Index.End)
				if err != nil {
					return exprRes{}, err
				}
				res.code = append(res.code, start.code...)
				res.code = append(res.code, end.code...)
				tmp := c.newVar()
				c.use("slice")
				res.code = append(res.code, fmt.Sprintf("slice(%s, %s, %s, %s),", res.val, start.val, end.val, tmp))
				res.val = tmp
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return exprRes{}, err
				}
				res.code = append(res.code, idx.code...)
				tmp := c.newVar()
				c.use("getitem")
				res.code = append(res.code, fmt.Sprintf("get_item(%s, %s, %s),", res.val, idx.val, tmp))
				res.val = tmp
			}
		} else if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				ar, err := c.compileExpr(a)
				if err != nil {
					return exprRes{}, err
				}
				res.code = append(res.code, ar.code...)
				args[i] = ar.val
			}
			tmp := c.newVar()
			res.code = append(res.code, fmt.Sprintf("%s(%s, %s),", sanitizeAtom(res.val), strings.Join(args, ", "), tmp))
			res.val = tmp
		} else if op.Cast != nil {
			continue
		} else {
			return exprRes{}, fmt.Errorf("unsupported postfix op")
		}
	}
	return res, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (exprRes, error) {
	switch {
	case p.Selector != nil:
		if len(p.Selector.Tail) != 0 {
			return exprRes{}, fmt.Errorf("selectors not supported")
		}
		if name, ok := c.vars[p.Selector.Root]; ok {
			tmp := c.newVar()
			line := fmt.Sprintf("nb_getval(%s, %s)", name, tmp)
			return exprRes{code: []string{line + ","}, val: tmp}, nil
		}
		return exprRes{val: sanitizeVar(p.Selector.Root)}, nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return exprRes{val: fmt.Sprintf("%d", *p.Lit.Int)}, nil
		}
		if p.Lit.Float != nil {
			return exprRes{val: fmt.Sprintf("%g", *p.Lit.Float)}, nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return exprRes{val: "true"}, nil
			}
			return exprRes{val: "false"}, nil
		}
		if p.Lit.Str != nil {
			return exprRes{val: fmt.Sprintf("%q", *p.Lit.Str)}, nil
		}
	case p.List != nil:
		elems := []string{}
		code := []string{}
		for _, e := range p.List.Elems {
			er, err := c.compileExpr(e)
			if err != nil {
				return exprRes{}, err
			}
			code = append(code, er.code...)
			elems = append(elems, er.val)
		}
		return exprRes{code: code, val: "[" + strings.Join(elems, ", ") + "]"}, nil
	case p.Map != nil:
		items := []string{}
		code := []string{}
		for _, it := range p.Map.Items {
			kr, err := c.compileExpr(it.Key)
			if err != nil {
				return exprRes{}, err
			}
			if len(kr.code) != 0 {
				return exprRes{}, fmt.Errorf("unsupported map key")
			}
			vr, err := c.compileExpr(it.Value)
			if err != nil {
				return exprRes{}, err
			}
			code = append(code, vr.code...)
			items = append(items, fmt.Sprintf("%s:%s", kr.val, vr.val))
		}
		return exprRes{code: code, val: "_{" + strings.Join(items, ", ") + "}"}, nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	}
	return exprRes{}, fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (exprRes, error) {
	switch call.Func {
	case "len":
		if len(call.Args) != 1 {
			return exprRes{}, fmt.Errorf("len expects 1 arg")
		}
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		tmp := c.newVar()
		code := append(arg.code, fmt.Sprintf("length(%s, %s),", arg.val, tmp))
		return exprRes{code: code, val: tmp}, nil
	default:
		args := make([]string, len(call.Args))
		code := []string{}
		for i, a := range call.Args {
			ar, err := c.compileExpr(a)
			if err != nil {
				return exprRes{}, err
			}
			code = append(code, ar.code...)
			args[i] = ar.val
		}
		tmp := c.newVar()
		callLine := fmt.Sprintf("%s(%s, %s)", sanitizeAtom(call.Func), strings.Join(args, ", "), tmp)
		code = append(code, callLine+",")
		return exprRes{code: code, val: tmp}, nil
	}
}
