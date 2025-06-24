package mlcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into OCaml source code (very limited subset).
type Compiler struct {
	pre     bytes.Buffer
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	tmp     int
	vars    map[string]bool
	mapVars map[string]bool
	setNth  bool
	slice   bool
	input   bool
	loopTmp int
	loops   []loopCtx
	funTmp  int
}

type loopCtx struct {
	brk  string
	cont string
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

func hasLoopCtrlIf(ifst *parser.IfStmt) bool {
	if ifst == nil {
		return false
	}
	if hasLoopCtrl(ifst.Then) || hasLoopCtrl(ifst.Else) {
		return true
	}
	return hasLoopCtrlIf(ifst.ElseIf)
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

// New creates a new OCaml compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:     env,
		vars:    map[string]bool{},
		mapVars: map[string]bool{},
		loops:   []loopCtx{},
	}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) newLoopID() string {
	id := fmt.Sprintf("%d", c.loopTmp)
	c.loopTmp++
	return id
}

func (c *Compiler) pushLoop(brk, cont string) {
	c.loops = append(c.loops, loopCtx{brk: brk, cont: cont})
}

func (c *Compiler) popLoop() {
	if len(c.loops) > 0 {
		c.loops = c.loops[:len(c.loops)-1]
	}
}

// Compile returns OCaml source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	if programHasLoopCtrl(prog.Statements) {
		c.pre.WriteString("exception BreakException of int\n")
		c.pre.WriteString("exception ContinueException of int\n\n")
	}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	for _, s := range prog.Statements {
		if s.Fun == nil {
			if err := c.compileStmt(s, ""); err != nil {
				return nil, err
			}
		}
	}
	if c.setNth || c.slice || c.input {
		c.writeln("")
	}
	var out bytes.Buffer
	out.Write(c.pre.Bytes())
	out.Write(c.buf.Bytes())
	return out.Bytes(), nil
}

func (c *Compiler) ensureSetNth() {
	if c.setNth {
		return
	}
	c.setNth = true
	b := &c.pre
	b.WriteString("let rec _set_nth lst i v =\n")
	b.WriteString("  match lst with\n")
	b.WriteString("  | [] -> []\n")
	b.WriteString("  | x::xs ->\n")
	b.WriteString("      if i = 0 then v :: xs else x :: _set_nth xs (i - 1) v\n\n")
}

func (c *Compiler) ensureSlice() {
	if c.slice {
		return
	}
	c.slice = true
	b := &c.pre
	b.WriteString("let rec _slice lst i len =\n")
	b.WriteString("  match lst with\n")
	b.WriteString("  | [] -> []\n")
	b.WriteString("  | x::xs ->\n")
	b.WriteString("      if i > 0 then _slice xs (i - 1) len\n")
	b.WriteString("      else if len = 0 then []\n")
	b.WriteString("      else x :: _slice xs 0 (len - 1)\n\n")
}

func (c *Compiler) ensureInput() {
	if c.input {
		return
	}
	c.input = true
	b := &c.pre
	b.WriteString("let _input () = read_line ();;\n\n")
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	ex := fmt.Sprintf("Return_%d", c.tmp)
	c.tmp++
	retTyp := ocamlType(fn.Return)
	if retTyp == "" {
		c.writeln(fmt.Sprintf("exception %s", ex))
	} else {
		c.writeln(fmt.Sprintf("exception %s of %s", ex, retTyp))
	}
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	// local environment with parameter types
	origEnv := c.env
	c.env = types.NewEnv(origEnv)
	for _, p := range fn.Params {
		if p.Type != nil && p.Type.Simple != nil {
			var t types.Type
			switch *p.Type.Simple {
			case "int":
				t = types.IntType{}
			case "float":
				t = types.FloatType{}
			case "string":
				t = types.StringType{}
			case "bool":
				t = types.BoolType{}
			}
			if t != nil {
				c.env.SetVar(p.Name, t, false)
			}
		}
	}
	defer func() { c.env = origEnv }()
	c.writeln(fmt.Sprintf("let rec %s %s =", sanitizeName(fn.Name), strings.Join(params, " ")))
	c.indent++
	c.writeln("try")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st, ex); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(fmt.Sprintf("with %s v -> v", ex))
	c.indent--
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement, ex string) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		if ex == "" {
			c.writeln(fmt.Sprintf("let %s = %s;;", sanitizeName(s.Let.Name), val))
		} else {
			c.writeln(fmt.Sprintf("let %s = %s in", sanitizeName(s.Let.Name), val))
		}
	case s.Var != nil:
		val := "()"
		if s.Var.Value != nil {
			var err error
			val, err = c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
		}
		name := sanitizeName(s.Var.Name)
		if c.vars == nil {
			c.vars = map[string]bool{}
		}
		c.vars[name] = true
		if s.Var.Type != nil && s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "map" {
			if c.mapVars == nil {
				c.mapVars = map[string]bool{}
			}
			c.mapVars[name] = true
		} else if s.Var.Value != nil && s.Var.Value.Binary != nil && s.Var.Value.Binary.Left != nil && s.Var.Value.Binary.Left.Value != nil && s.Var.Value.Binary.Left.Value.Target != nil && s.Var.Value.Binary.Left.Value.Target.Map != nil {
			if c.mapVars == nil {
				c.mapVars = map[string]bool{}
			}
			c.mapVars[name] = true
		}
		if strings.HasPrefix(val, "fun ") || strings.HasPrefix(val, "fun(") {
			val = "(" + val + ")"
		}
		if ex == "" {
			c.writeln(fmt.Sprintf("let %s = ref %s;;", name, val))
		} else {
			c.writeln(fmt.Sprintf("let %s = ref %s in", name, val))
		}
	case s.Assign != nil:
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		name := sanitizeName(s.Assign.Name)
		target := name
		if c.vars[name] {
			target = "!" + name
		}
		if len(s.Assign.Index) > 0 {
			idx, err := c.compileExpr(s.Assign.Index[0].Start)
			if err != nil {
				return err
			}
			if c.mapVars[name] {
				if ex == "" {
					c.writeln(fmt.Sprintf("Hashtbl.replace %s %s %s;;", target, idx, val))
				} else {
					c.writeln(fmt.Sprintf("Hashtbl.replace %s %s %s;", target, idx, val))
				}
				return nil
			} else if c.vars[name] {
				c.ensureSetNth()
				if ex == "" {
					c.writeln(fmt.Sprintf("%s := _set_nth %s %s (%s);;", name, target, idx, val))
				} else {
					c.writeln(fmt.Sprintf("%s := _set_nth %s %s (%s);", name, target, idx, val))
				}
				return nil
			}
		}
		if ex == "" {
			c.writeln(fmt.Sprintf("%s := %s;;", name, val))
		} else {
			c.writeln(fmt.Sprintf("%s := %s;", name, val))
		}
	case s.Fun != nil:
		if err := c.compileFun(s.Fun); err != nil {
			return err
		}
		if ex != "" {
			c.writeln("in")
		} else {
			c.writeln("")
		}
	case s.While != nil:
		return c.compileWhile(s.While, ex)
	case s.Return != nil:
		if ex == "" {
			return fmt.Errorf("return outside function")
		}
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("raise (%s (%s))", ex, val))
	case s.For != nil:
		return c.compileFor(s.For, ex)
	case s.If != nil:
		return c.compileIf(s.If, ex)
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
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if ex == "" {
			c.writeln(expr + ";;")
		} else {
			c.writeln(expr + ";")
		}
	default:
		// ignore other statements
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt, ex string) error {
	// create scoped environment with loop variable type for proper codegen
	origEnv := c.env
	if origEnv != nil {
		child := types.NewEnv(origEnv)
		if f.RangeEnd != nil {
			// numeric range loops yield ints
			child.SetVar(f.Name, types.IntType{}, true)
		} else if isStringExpr(f.Source, origEnv) {
			child.SetVar(f.Name, types.StringType{}, true)
		} else if isListExpr(f.Source, origEnv) {
			child.SetVar(f.Name, types.AnyType{}, true)
		} else if isMapExpr(f.Source, origEnv) {
			child.SetVar(f.Name, types.AnyType{}, true)
		} else {
			child.SetVar(f.Name, types.AnyType{}, true)
		}
		c.env = child
		defer func() { c.env = origEnv }()
	}
	ctrl := hasLoopCtrl(f.Body)
	var id string
	var brk, cont string
	if ctrl {
		id = c.newLoopID()
		brk = fmt.Sprintf("BreakException %s", id)
		cont = fmt.Sprintf("ContinueException %s", id)
		c.writeln("try")
		c.indent++
	}
	if f.RangeEnd == nil {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		iter := "List.iter"
		if isStringExpr(f.Source, c.env) {
			iter = "String.iter"
		}
		c.writeln(fmt.Sprintf("%s (fun %s ->", iter, sanitizeName(f.Name)))
		c.indent++
		if ctrl {
			c.pushLoop(brk, cont)
			c.writeln("try")
			c.indent++
		}
		bodyEx := ex
		if bodyEx == "" {
			bodyEx = "loop"
		}
		for _, st := range f.Body {
			if err := c.compileStmt(st, bodyEx); err != nil {
				return err
			}
		}
		if ctrl {
			c.indent--
			c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
			c.popLoop()
		}
		c.indent--
		if ex == "" {
			if ctrl {
				c.writeln(fmt.Sprintf(") %s;", src))
			} else {
				c.writeln(fmt.Sprintf(") %s;;", src))
			}
		} else {
			c.writeln(fmt.Sprintf(") %s;", src))
		}
		if ctrl {
			c.indent--
			if ex == "" {
				c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ();;", id))
			} else {
				c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
			}
		}
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
	c.writeln(fmt.Sprintf("for %s = %s to %s - 1 do", sanitizeName(f.Name), start, end))
	c.indent++
	if ctrl {
		c.pushLoop(brk, cont)
		c.writeln("try")
		c.indent++
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st, ex); err != nil {
			return err
		}
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
		c.popLoop()
	}
	c.indent--
	if ex == "" {
		if ctrl {
			c.writeln("done;")
		} else {
			c.writeln("done;;")
		}
	} else {
		c.writeln("done;")
	}
	if ctrl {
		c.indent--
		if ex == "" {
			c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ();", id))
		} else {
			c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
		}
	}
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt, ex string) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	ctrl := hasLoopCtrl(w.Body)
	var id string
	var brk, cont string
	if ctrl {
		id = c.newLoopID()
		brk = fmt.Sprintf("BreakException %s", id)
		cont = fmt.Sprintf("ContinueException %s", id)
		c.writeln("try")
		c.indent++
	}
	c.writeln(fmt.Sprintf("while %s do", cond))
	c.indent++
	bodyEx := ex
	if bodyEx == "" {
		bodyEx = "loop"
	}
	if ctrl {
		c.pushLoop(brk, cont)
		c.writeln("try")
		c.indent++
	}
	for _, st := range w.Body {
		if err := c.compileStmt(st, bodyEx); err != nil {
			return err
		}
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with ContinueException n when n = %s -> ()", id))
		c.popLoop()
	}
	c.indent--
	if ex == "" {
		c.writeln("done;;")
	} else {
		c.writeln("done;")
	}
	if ctrl {
		c.indent--
		c.writeln(fmt.Sprintf("with BreakException n when n = %s -> ()", id))
	}
	return nil
}

func (c *Compiler) compileIf(ifst *parser.IfStmt, ex string) error {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s then begin", cond))
	c.indent++
	for _, st := range ifst.Then {
		if err := c.compileStmt(st, ex); err != nil {
			return err
		}
	}
	c.indent--
	if ifst.ElseIf != nil {
		c.writeln("end else begin")
		c.indent++
		if err := c.compileIf(ifst.ElseIf, ex); err != nil {
			return err
		}
		c.indent--
		if ex == "" {
			c.writeln("end")
		} else {
			c.writeln("end;")
		}
	} else if len(ifst.Else) > 0 {
		c.writeln("end else begin")
		c.indent++
		for _, st := range ifst.Else {
			if err := c.compileStmt(st, ex); err != nil {
				return err
			}
		}
		c.indent--
		if ex == "" {
			c.writeln("end")
		} else {
			c.writeln("end;")
		}
	} else {
		if ex == "" {
			c.writeln("end")
		} else {
			c.writeln("end;")
		}
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "()", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	expr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		oper := op.Op
		if oper == "==" {
			oper = "="
		} else if oper == "!=" {
			oper = "<>"
		} else if oper == "%" {
			oper = "mod"
		} else if oper == "+" {
			// use list concatenation or string concat if operand is list or string
			if isListExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env) {
				oper = "@"
			} else if isStringExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env) || isStringExpr(&parser.Expr{Binary: b}, c.env) {
				oper = "^"
				// convert char from String.get to string for concatenation
				if strings.HasPrefix(r, "(String.get ") {
					r = fmt.Sprintf("(String.make 1 %s)", r)
				}
				if strings.HasPrefix(expr, "(String.get ") {
					expr = fmt.Sprintf("(String.make 1 %s)", expr)
				}
			}
		} else if oper == "/" {
			if isFloatExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env) || isFloatExpr(&parser.Expr{Binary: b}, c.env) {
				oper = "/."
			}
		} else if oper == "in" {
			expr = fmt.Sprintf("Hashtbl.mem %s %s", r, expr)
			continue
		}
		expr = fmt.Sprintf("%s %s %s", expr, oper, r)
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
		if op == "!" {
			expr = fmt.Sprintf("not %s", expr)
		} else if op == "-" && (strings.HasPrefix(expr, "!") || strings.HasPrefix(expr, "not ")) {
			expr = fmt.Sprintf("-(%s)", expr)
		} else {
			expr = fmt.Sprintf("%s%s", op, expr)
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
		} else if op.Index != nil {
			start, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if op.Index.Colon != nil {
				end := ""
				if op.Index.End != nil {
					end, err = c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
				}
				if isStringPrimary(p.Target, c.env) {
					if end == "" {
						expr = fmt.Sprintf("(String.sub %s %s (String.length %s - %s))", expr, start, expr, start)
					} else {
						expr = fmt.Sprintf("(String.sub %s %s (%s - %s))", expr, start, end, start)
					}
				} else {
					base := expr
					var length string
					if end == "" {
						length = fmt.Sprintf("(List.length %s - %s)", base, start)
					} else {
						length = fmt.Sprintf("(%s - %s)", end, start)
					}
					c.ensureSlice()
					expr = fmt.Sprintf("(_slice %s %s %s)", base, start, length)
				}
			} else {
				if isStringPrimary(p.Target, c.env) {
					expr = fmt.Sprintf("(String.get %s %s)", expr, start)
				} else if isMapPrimary(p.Target, c.env) {
					expr = fmt.Sprintf("(Hashtbl.find %s %s)", expr, start)
				} else {
					expr = fmt.Sprintf("(List.nth %s %s)", expr, start)
				}
			}
		} else if op.Cast != nil {
			typ := ocamlType(op.Cast.Type)
			if typ == "float" {
				expr = fmt.Sprintf("(float_of_int %s)", expr)
			} else if typ != "" {
				expr = fmt.Sprintf("(%s : %s)", expr, typ)
			}
		}
	}
	return expr, nil
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
				s += "."
			}
			return s, nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		return "0", nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "[" + strings.Join(elems, "; ") + "]", nil
	case p.Map != nil:
		var b strings.Builder
		b.WriteString("(let tbl = Hashtbl.create ")
		b.WriteString(strconv.Itoa(len(p.Map.Items)))
		b.WriteString(" in ")
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			b.WriteString("Hashtbl.add tbl ")
			b.WriteString(k)
			b.WriteString(" ")
			b.WriteString(v)
			if i < len(p.Map.Items)-1 {
				b.WriteString(";")
			}
		}
		if len(p.Map.Items) > 0 {
			b.WriteString("; ")
		}
		b.WriteString("tbl)")
		return b.String(), nil
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if c.vars[name] {
			name = "!" + name
		}
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		}
		return name, nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("fun %s -> %s", strings.Join(params, " "), body), nil
	}
	c.funTmp++
	origEnv := c.env
	c.env = types.NewEnv(origEnv)
	for _, p := range fn.Params {
		if p.Type != nil && p.Type.Simple != nil {
			var t types.Type
			switch *p.Type.Simple {
			case "int":
				t = types.IntType{}
			case "float":
				t = types.FloatType{}
			case "string":
				t = types.StringType{}
			case "bool":
				t = types.BoolType{}
			}
			if t != nil {
				c.env.SetVar(p.Name, t, false)
			}
		}
	}
	ex := fmt.Sprintf("Return_%d", c.tmp)
	c.tmp++
	sub := &Compiler{env: c.env, indent: 2, vars: map[string]bool{}, mapVars: map[string]bool{}}
	for _, st := range fn.BlockBody {
		if err := sub.compileStmt(st, ex); err != nil {
			c.env = origEnv
			return "", err
		}
	}
	if sub.setNth {
		c.ensureSetNth()
	}
	if sub.slice {
		c.ensureSlice()
	}
	if sub.input {
		c.ensureInput()
	}
	body := strings.TrimRight(sub.buf.String(), "\n")
	c.env = origEnv
	var out strings.Builder
	out.WriteString("fun ")
	out.WriteString(strings.Join(params, " "))
	out.WriteString(" ->\n  let exception ")
	retTyp := ocamlType(fn.Return)
	if retTyp == "" {
		out.WriteString(ex)
	} else {
		out.WriteString(ex + " of " + retTyp)
	}
	out.WriteString(" in\n  try\n")
	out.WriteString(body)
	if body != "" && !strings.HasSuffix(body, "\n") {
		out.WriteByte('\n')
	}
	out.WriteString("  with ")
	out.WriteString(ex)
	out.WriteString(" v -> v")
	return out.String(), nil
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
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
		if len(args) == 1 {
			if isStringExpr(call.Args[0], c.env) {
				return fmt.Sprintf("String.length %s", args[0]), nil
			}
			if isMapExpr(call.Args[0], c.env) {
				return fmt.Sprintf("Hashtbl.length %s", args[0]), nil
			}
			return fmt.Sprintf("List.length %s", args[0]), nil
		}
	case "print":
		if len(args) == 1 {
			if isStringExpr(call.Args[0], c.env) {
				return fmt.Sprintf("print_endline (%s)", args[0]), nil
			}
			if isBoolExpr(call.Args[0], c.env) {
				return fmt.Sprintf("print_endline (string_of_bool (%s))", args[0]), nil
			}
			if isFloatExpr(call.Args[0], c.env) {
				return fmt.Sprintf("print_endline (string_of_float (%s))", args[0]), nil
			}
			return fmt.Sprintf("print_endline (string_of_int (%s))", args[0]), nil
		}
		if len(args) > 0 {
			parts := make([]string, len(args))
			for i, a := range call.Args {
				if isStringExpr(a, c.env) {
					parts[i] = args[i]
				} else if isBoolExpr(a, c.env) {
					parts[i] = fmt.Sprintf("string_of_bool (%s)", args[i])
				} else if isFloatExpr(a, c.env) {
					parts[i] = fmt.Sprintf("string_of_float (%s)", args[i])
				} else {
					parts[i] = fmt.Sprintf("string_of_int (%s)", args[i])
				}
			}
			return fmt.Sprintf("print_endline (String.concat \" \" [%s])", strings.Join(parts, "; ")), nil
		}
	case "str":
		if len(args) == 1 {
			if isStringExpr(call.Args[0], c.env) {
				return args[0], nil
			}
			if isFloatExpr(call.Args[0], c.env) {
				return fmt.Sprintf("string_of_float (%s)", args[0]), nil
			}
			return fmt.Sprintf("string_of_int (%s)", args[0]), nil
		}
	case "input":
		if len(args) == 0 {
			c.ensureInput()
			return "_input ()", nil
		}
	}
	name := sanitizeName(call.Func)
	if c.vars[name] {
		name = "!" + name
	}
	return fmt.Sprintf("%s %s", name, strings.Join(args, " ")), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	parts := make([]string, len(m.Cases))
	for i, cs := range m.Cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if pat == "_" {
			parts[i] = fmt.Sprintf("_ -> %s", res)
		} else {
			parts[i] = fmt.Sprintf("%s -> %s", pat, res)
		}
	}
	return fmt.Sprintf("(match %s with %s)", target, strings.Join(parts, " | ")), nil
}

func ocamlType(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "float"
		case "string":
			return "string"
		case "bool":
			return "bool"
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			elem := ocamlType(t.Generic.Args[0])
			if elem == "" {
				elem = "unit"
			}
			return fmt.Sprintf("%s list", elem)
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			key := ocamlType(t.Generic.Args[0])
			if key == "" {
				key = "unit"
			}
			val := ocamlType(t.Generic.Args[1])
			if val == "" {
				val = "unit"
			}
			return fmt.Sprintf("(%s, %s) Hashtbl.t", key, val)
		}
	}
	return ""
}

func isStringExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func isListExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.List != nil {
		return true
	}
	if p.Selector != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(types.ListType); ok {
				return true
			}
		}
	}
	return false
}

func isFloatExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Float != nil {
		return true
	}
	if p.Selector != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(types.FloatType); ok {
				return true
			}
		}
	}
	return false
}

func isBoolExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Bool != nil {
		return true
	}
	if p.Selector != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(types.BoolType); ok {
				return true
			}
		}
	}
	return false
}

func isMapExpr(e *parser.Expr, env *types.Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func isMapPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func isStringPrimary(p *parser.Primary, env *types.Env) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	res := b.String()
	if res == "" || !((res[0] >= 'A' && res[0] <= 'Z') || (res[0] >= 'a' && res[0] <= 'z') || res[0] == '_') {
		res = "_" + res
	}
	switch res {
	case "end", "type", "module", "let", "in", "match", "with", "and", "or":
		res = "_" + res
	}
	return res
}
