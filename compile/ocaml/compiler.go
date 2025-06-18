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
	buf      bytes.Buffer
	indent   int
	env      *types.Env
	tmp      int
	vars     map[string]bool
	charVars map[string]int
}

// New creates a new OCaml compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, vars: map[string]bool{}, charVars: map[string]int{}}
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

// Compile returns OCaml source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
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
	return c.buf.Bytes(), nil
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
		if c.env != nil {
			var t types.Type
			if s.Var.Type != nil {
				t = resolveTypeRef(s.Var.Type, c.env)
			} else if isStringExpr(s.Var.Value, c.env) {
				t = types.StringType{}
			}
			if t != nil {
				c.env.SetVar(s.Var.Name, t, true)
			}
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
		if len(s.Assign.Index) == 0 {
			if ex == "" {
				c.writeln(fmt.Sprintf("%s := %s;;", name, val))
			} else {
				c.writeln(fmt.Sprintf("%s := %s;", name, val))
			}
		} else {
			idx, err := c.compileExpr(s.Assign.Index[0].Start)
			if err != nil {
				return err
			}
			tmp := fmt.Sprintf("tmp_%d", c.tmp)
			c.tmp++
			c.writeln(fmt.Sprintf("let %s = Array.of_list !%s in", tmp, name))
			c.writeln(fmt.Sprintf("%s.(%s) <- %s;", tmp, idx, val))
			if ex == "" {
				c.writeln(fmt.Sprintf("%s := Array.to_list %s;;", name, tmp))
			} else {
				c.writeln(fmt.Sprintf("%s := Array.to_list %s;", name, tmp))
			}
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
	if f.RangeEnd == nil {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		iter := "List.iter"
		var elem types.Type
		if isStringExpr(f.Source, c.env) {
			iter = "String.iter"
			elem = types.StringType{}
		}
		origEnv := c.env
		if elem != nil {
			c.env = types.NewEnv(origEnv)
			c.env.SetVar(f.Name, elem, false)
			c.charVars[sanitizeName(f.Name)]++
		}
		c.writeln(fmt.Sprintf("%s (fun %s ->", iter, sanitizeName(f.Name)))
		c.indent++
		for _, st := range f.Body {
			if err := c.compileStmt(st, ex); err != nil {
				if elem != nil {
					c.env = origEnv
					if c.charVars[sanitizeName(f.Name)] > 0 {
						c.charVars[sanitizeName(f.Name)]--
					}
				}
				return err
			}
		}
		c.indent--
		if elem != nil {
			c.env = origEnv
		}
		if ex == "" {
			c.writeln(fmt.Sprintf(") %s;;", src))
		} else {
			c.writeln(fmt.Sprintf(") %s;", src))
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
	for _, st := range f.Body {
		if err := c.compileStmt(st, ex); err != nil {
			return err
		}
	}
	c.indent--
	if ex == "" {
		c.writeln("done;;")
	} else {
		c.writeln("done;")
	}
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt, ex string) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while %s do", cond))
	c.indent++
	bodyEx := ex
	if bodyEx == "" {
		bodyEx = "loop"
	}
	for _, st := range w.Body {
		if err := c.compileStmt(st, bodyEx); err != nil {
			return err
		}
	}
	c.indent--
	if ex == "" {
		c.writeln("done;;")
	} else {
		c.writeln("done;")
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
			leftIsStr := isStringExpr(&parser.Expr{Binary: b}, c.env)
			rightIsStr := isStringExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env)
			if isListExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env) {
				oper = "@"
			} else if leftIsStr || rightIsStr {
				oper = "^"
				// convert char from String.get or loop variable to string for concatenation
				if strings.HasPrefix(r, "(String.get ") || c.charVars[r] > 0 {
					r = fmt.Sprintf("(String.make 1 %s)", r)
				}
			}
		} else if oper == "/" {
			if isFloatExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}}, c.env) || isFloatExpr(&parser.Expr{Binary: b}, c.env) {
				oper = "/."
			}
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
		expr = fmt.Sprintf("%s%s", u.Ops[i], expr)
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
					expr = fmt.Sprintf("(List.nth %s %s) (* slice unsupported *)", expr, start)
				}
			} else {
				if isStringPrimary(p.Target, c.env) {
					expr = fmt.Sprintf("(String.get %s %s)", expr, start)
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
	return "", fmt.Errorf("unsupported function expression")
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
			return fmt.Sprintf("List.length %s", args[0]), nil
		}
	case "print":
		if len(args) == 1 {
			if isStringExpr(call.Args[0], c.env) {
				return fmt.Sprintf("print_endline %s", args[0]), nil
			}
			return fmt.Sprintf("print_endline (string_of_int (%s))", args[0]), nil
		}
	}
	return fmt.Sprintf("%s %s", sanitizeName(call.Func), strings.Join(args, " ")), nil
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

func resolveTypeRef(t *parser.TypeRef, env *types.Env) types.Type {
	if t == nil {
		return types.IntType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		}
	}
	if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
		return types.ListType{Elem: resolveTypeRef(t.Generic.Args[0], env)}
	}
	return types.AnyType{}
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
