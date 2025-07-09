//go:build slow

package ocaml

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a minimal subset of Mochi to OCaml.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	vars   map[string]bool // variables declared with 'var'

	loop int
	env  *types.Env
}

// New creates a compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{vars: make(map[string]bool), env: env}
}

// Compile emits OCaml code for prog. Only a few constructs are supported.
func (c *Compiler) Compile(prog *parser.Program, _ string) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0

	// helper pretty printer for `print` builtin
	c.writeln("let rec __show v =")
	c.indent++
	c.writeln("let open Obj in")
	c.writeln("let rec list_aux o =")
	c.indent++
	c.writeln("if is_int o && (magic (obj o) : int) = 0 then \"\" else")
	c.writeln(" let hd = field o 0 in")
	c.writeln(" let tl = field o 1 in")
	c.writeln(" let rest = list_aux tl in")
	c.writeln(" if rest = \"\" then __show (obj hd) else __show (obj hd) ^ \"; \" ^ rest")
	c.indent--
	c.writeln("in")
	c.writeln("let r = repr v in")
	c.writeln("if is_int r then string_of_int (magic v) else")
	c.writeln("match tag r with")
	c.indent++
	c.writeln("| 0 -> if size r = 0 then \"[]\" else \"[\" ^ list_aux r ^ \"]\"")
	c.writeln("| 252 -> (magic v : string)")
	c.writeln("| 253 -> string_of_float (magic v)")
	c.writeln("| _ -> \"<value>\"")
	c.indent--
	c.indent--
	c.buf.WriteByte('\n')

	c.writeln("exception Break")
	c.writeln("exception Continue")
	c.buf.WriteByte('\n')

	// helper for substring search used by the `contains` method
	c.writeln("let string_contains s sub =")
	c.indent++
	c.writeln("let len_s = String.length s and len_sub = String.length sub in")
	c.writeln("let rec aux i =")
	c.indent++
	c.writeln("if i + len_sub > len_s then false")
	c.writeln("else if String.sub s i len_sub = sub then true")
	c.writeln("else aux (i + 1)")
	c.indent--
	c.writeln("in aux 0")
	c.indent--
	c.buf.WriteByte('\n')

	// first emit type, function and variable declarations
	for _, s := range prog.Statements {
		switch {
		case s.Type != nil:
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
		case s.Fun != nil:
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.buf.WriteByte('\n')
		case s.Let != nil:
			if err := c.compileGlobalLet(s.Let); err != nil {
				return nil, err
			}
		case s.Var != nil:
			if err := c.compileGlobalVar(s.Var); err != nil {
				return nil, err
			}
		}
	}

	c.buf.WriteByte('\n')
	c.writeln("let () =")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Let != nil || s.Var != nil || s.Type != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	if c.buf.Len() == 0 {
		c.writeln("()")
	}
	c.indent--
	return c.buf.Bytes(), nil
}

// --- statements ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ";")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(val)
		return nil
	case s.Break != nil:
		c.writeln("raise Break")
		return nil
	case s.Continue != nil:
		c.writeln("raise Continue")
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileGlobalLet(l *parser.LetStmt) error {
	val := "()"
	var err error
	if l.Value != nil {
		val, err = c.compileExpr(l.Value)
		if err != nil {
			return err
		}
	} else if l.Type != nil && l.Type.Simple != nil {
		switch *l.Type.Simple {
		case "int":
			val = "0"
		case "float":
			val = "0.0"
		case "bool":
			val = "false"
		case "string":
			val = "\"\""
		}
	}
	c.writeln(fmt.Sprintf("let %s = %s", l.Name, val))
	return nil
}

func (c *Compiler) compileGlobalVar(v *parser.VarStmt) error {
	val := "0"
	var err error
	if v.Value != nil {
		val, err = c.compileExpr(v.Value)
		if err != nil {
			return err
		}
	}
	c.vars[v.Name] = true
	c.writeln(fmt.Sprintf("let %s = ref %s", v.Name, val))
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 || len(t.Members) == 0 {
		// unions or empty types unsupported
		return nil
	}
	fields := make([]string, 0, len(t.Members))
	for _, m := range t.Members {
		if m.Field == nil {
			continue
		}
		typ := c.typeRef(m.Field.Type)
		fields = append(fields, fmt.Sprintf("%s : %s", m.Field.Name, typ))
	}
	name := strings.ToLower(t.Name)
	c.writeln(fmt.Sprintf("type %s = { %s }", name, strings.Join(fields, "; ")))
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	if len(a.Index) > 0 && c.vars[a.Name] {
		idx, err := c.compileExpr(a.Index[0].Start)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := (let rec __upd l = match l with | [] -> [(%s,%s)] | (k,v)::tl -> if k = %s then (%s,%s)::tl else (k,v)::__upd tl in __upd !%s);", a.Name, idx, val, idx, idx, val, a.Name))
		return nil
	}
	if c.vars[a.Name] {
		c.writeln(fmt.Sprintf("%s := %s;", a.Name, val))
	} else {
		c.writeln(fmt.Sprintf("(* assignment to %s unsupported *)", a.Name))
	}
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s then (", cond))
	c.indent++
	for _, st := range i.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.ElseIf != nil {
		c.writeln(") else (")
		c.indent++
		if err := c.compileIf(i.ElseIf); err != nil {
			return err
		}
		c.indent--
		c.writeln(")")
	} else if len(i.Else) > 0 {
		c.writeln(") else (")
		c.indent++
		for _, st := range i.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	} else {
		c.writeln(")")
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
		loopName := fmt.Sprintf("__loop%d", c.loop)
		c.loop++
		c.writeln(fmt.Sprintf("let rec %s i =", loopName))
		c.indent++
		c.writeln(fmt.Sprintf("if i > %s then () else (", end))
		c.indent++
		c.writeln("try")
		c.indent++
		c.writeln(fmt.Sprintf("let %s = i in", fr.Name))
		for _, st := range fr.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("with Continue -> ()")
		c.writeln(fmt.Sprintf("; %s (i + 1))", loopName))
		c.indent--
		c.indent--
		c.writeln("in")
		c.writeln(fmt.Sprintf("try %s %s with Break -> ()", loopName, start))
		return nil
	}
	src, err := c.compileExpr(fr.Source)
	if err != nil {
		return err
	}
	loopName := fmt.Sprintf("__loop%d", c.loop)
	c.loop++
	c.writeln(fmt.Sprintf("let rec %s lst =", loopName))
	c.indent++
	c.writeln("match lst with")
	c.indent++
	c.writeln("| [] -> ()")
	c.writeln(fmt.Sprintf("| %s::rest ->", fr.Name))
	c.indent++
	c.writeln("try")
	c.indent++
	for _, st := range fr.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("with Continue -> ()")
	c.writeln(fmt.Sprintf("; %s rest", loopName))
	c.indent--
	c.indent--
	c.writeln("in")
	c.writeln(fmt.Sprintf("try %s %s with Break -> ()", loopName, src))
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	loopName := fmt.Sprintf("__loop%d", c.loop)
	c.loop++
	c.writeln(fmt.Sprintf("let rec %s () =", loopName))
	c.indent++
	c.writeln(fmt.Sprintf("if %s then (", cond))
	c.indent++
	c.writeln("try")
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln(fmt.Sprintf("%s ()", loopName))
	c.indent--
	c.writeln("with Continue -> ()")
	c.indent--
	c.writeln(") else ()")
	c.indent--
	c.writeln(fmt.Sprintf("in try %s () with Break -> ()", loopName))
	return nil
}

// --- expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "()", fmt.Errorf("empty expression")
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
		case "==":
			opStr = "="
		case "!=":
			opStr = "<>"
		case "%":
			opStr = "mod"
		case "in":
			if c.isMapPostfix(op.Right) {
				res = fmt.Sprintf("(List.mem_assoc %s %s)", res, r)
			} else {
				res = fmt.Sprintf("(List.mem %s %s)", res, r)
			}
			continue
		}
		if opStr == "+" && isStringUnary(b.Left) && isStringExprExpr(op.Right) {
			res = fmt.Sprintf("(%s ^ %s)", res, r)
		} else {
			res = fmt.Sprintf("(%s %s %s)", res, opStr, r)
		}
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
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := []string{}
			for _, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, s)
			}
			if strings.HasSuffix(val, ".contains") && len(args) == 1 {
				base := strings.TrimSuffix(val, ".contains")
				val = fmt.Sprintf("string_contains %s %s", base, args[0])
			} else {
				val = fmt.Sprintf("%s %s", val, strings.Join(args, " "))
			}
		case op.Index != nil:
			if op.Index.Start == nil {
				return "", fmt.Errorf("unsupported index")
			}
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if isStringPrimary(p.Target) {
				val = fmt.Sprintf("String.make 1 (String.get %s %s)", val, idx)
			} else if isMapPrimary(p.Target) || isStringExpr(op.Index.Start) {
				val = fmt.Sprintf("List.assoc %s %s", idx, val)
			} else {
				val = fmt.Sprintf("List.nth %s %s", val, idx)
			}
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				switch *op.Cast.Type.Simple {
				case "int":
					val = fmt.Sprintf("int_of_string %s", val)
				default:
					if p.Target.Map != nil {
						rec, err := c.recordLiteral(p.Target.Map)
						if err != nil {
							return "", err
						}
						val = rec
					} else {
						return "", fmt.Errorf("unsupported cast")
					}
				}
			} else {
				return "", fmt.Errorf("unsupported cast")
			}
		default:
			return "", fmt.Errorf("unsupported postfix")
		}
	}
	return val, nil
}

func (c *Compiler) recordLiteral(m *parser.MapLiteral) (string, error) {
	fields := make([]string, len(m.Items))
	for i, it := range m.Items {
		key, ok := stringConst(it.Key)
		if !ok {
			return "", fmt.Errorf("unsupported struct key")
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s = %s", key, v)
	}
	return "{ " + strings.Join(fields, "; ") + " }", nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Selector != nil:
		name := p.Selector.Root
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		if c.vars[name] {
			return "!" + name, nil
		}
		return name, nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[" + strings.Join(elems, ";") + "]", nil
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
			items[i] = fmt.Sprintf("(%s,%s)", k, v)
		}
		return "[" + strings.Join(items, ";") + "]", nil
	case p.FunExpr != nil:
		params := make([]string, len(p.FunExpr.Params))
		for i, p2 := range p.FunExpr.Params {
			params[i] = p2.Name
		}
		if p.FunExpr.ExprBody != nil {
			body, err := c.compileExpr(p.FunExpr.ExprBody)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("fun %s -> %s", strings.Join(params, " "), body), nil
		}
		if len(p.FunExpr.BlockBody) > 0 {
			var buf bytes.Buffer
			buf.WriteString("(fun " + strings.Join(params, " ") + " ->\n")
			c.indent++
			for _, st := range p.FunExpr.BlockBody {
				if err := c.compileStmt(st); err != nil {
					return "", err
				}
			}
			c.indent--
			buf.WriteString(")")
			return buf.String(), nil
		}
		return "fun _ -> ()", nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Group != nil:
		s, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", s), nil
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
	case "print":
		if len(args) == 0 {
			return "", fmt.Errorf("print expects at least 1 arg")
		}
		if len(args) == 1 {
			return fmt.Sprintf("print_endline (__show (%s))", args[0]), nil
		}
		parts := make([]string, len(args))
		for i, a := range args {
			parts[i] = fmt.Sprintf("__show (%s)", a)
		}
		expr := strings.Join(parts, " ^ \" \" ^ ")
		return fmt.Sprintf("print_endline (%s)", expr), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("(%s @ [%s])", args[0], args[1]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		if isStringLiteralExpr(call.Args[0]) {
			return fmt.Sprintf("String.length %s", args[0]), nil
		}
		return fmt.Sprintf("List.length %s", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("List.length %s", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(List.fold_left (+) 0 %s / List.length %s)", args[0], args[0]), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("String.sub %s %s (%s - %s)", args[0], args[1], args[2], args[1]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("__show (%s)", args[0]), nil
	default:
		return fmt.Sprintf("%s %s", call.Func, strings.Join(args, " ")), nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	case l.Null:
		return "()"
	default:
		return "()"
	}
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = p.Name
	}
	c.writeln(fmt.Sprintf("let rec %s %s =", fn.Name, strings.Join(params, " ")))
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if len(fn.Body) == 0 {
		c.writeln("()")
	}
	c.indent--
	return nil
}

func (c *Compiler) typeRef(t *parser.TypeRef) string {
	if t == nil {
		return "unit"
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
		default:
			return strings.ToLower(*t.Simple)
		}
	}
	return "unit"
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func isStringLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return false
	}
	if u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil {
		return true
	}
	return false
}

func isStringCall(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return false
	}
	if u.Value.Target.Call != nil && u.Value.Target.Call.Func == "str" {
		return true
	}
	return false
}

func isStringExpr(e *parser.Expr) bool {
	return isStringLiteralExpr(e) || isStringCall(e)
}

func isStringExprExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Call != nil && p.Target.Call.Func == "str" {
		return true
	}
	return false
}

func isStringPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Call != nil && p.Call.Func == "str" {
		return true
	}
	return false
}

func isMapPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	return p.Map != nil
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Map != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Ops) == 0 {
		if typ, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := typ.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func isStringUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	p := u.Value.Target
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Call != nil && p.Call.Func == "str" {
		return true
	}
	return false
}

func stringConst(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return "", false
	}
	if u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil {
		return *u.Value.Target.Lit.Str, true
	}
	return "", false
}
