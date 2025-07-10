//go:build slow

package pl

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

type Compiler struct {
	buf           bytes.Buffer
	funcBuf       bytes.Buffer
	lambdaBuf     bytes.Buffer
	out           *bytes.Buffer
	indent        int
	tmp           int
	vars          map[string]string
	retVar        string
	needsGetItem  bool
	needsSlice    bool
	needsContains bool
	needsLenAny   bool
	needsSetItem  bool
}

func New() *Compiler {
	c := &Compiler{vars: make(map[string]string)}
	c.out = &c.funcBuf
	return c
}

func (c *Compiler) newVar(base string) string {
	name := sanitizeVar(base)
	if _, ok := c.vars[base]; !ok {
		c.vars[base] = name
		return name
	}
	tmp := fmt.Sprintf("%s_%d", name, c.tmp)
	c.tmp++
	c.vars[base] = tmp
	return tmp
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.out.WriteString("    ")
	}
	c.out.WriteString(s)
	c.out.WriteByte('\n')
}

func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.funcBuf.Reset()
	c.lambdaBuf.Reset()
	c.indent = 0
	c.tmp = 0
	c.vars = make(map[string]string)
	c.needsGetItem = false
	c.needsSlice = false
	c.needsContains = false
	c.needsLenAny = false
	c.needsSetItem = false

	c.out = &c.funcBuf
	for _, st := range p.Statements {
		if st.Fun != nil {
			if err := c.compileFun(st.Fun); err != nil {
				return nil, err
			}
		}
	}
	c.out = &c.buf
	c.writeln(":- initialization(main, main).")
	c.writeln("main :-")
	c.indent++
	for _, st := range p.Statements {
		if st.Fun != nil {
			continue
		}
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	c.writeln("true.")
	c.indent--
	var out bytes.Buffer
	out.WriteString(":- style_check(-singleton).\n")
	if c.needsGetItem {
		out.WriteString("get_item(Container, Key, Val) :-\n")
		out.WriteString("    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), get_dict(A, Container, Val).\n")
		out.WriteString("get_item(Container, Index, Val) :-\n")
		out.WriteString("    string(Container), !, string_chars(Container, Chars), nth0(Index, Chars, Val).\n")
		out.WriteString("get_item(List, Index, Val) :- nth0(Index, List, Val).\n\n")
	}
	if c.needsSlice {
		out.WriteString("slice(Str, I, J, Out) :-\n")
		out.WriteString("    string(Str), !,\n")
		out.WriteString("    Len is J - I,\n")
		out.WriteString("    sub_string(Str, I, Len, _, Out).\n")
		out.WriteString("slice(List, I, J, Out) :-\n")
		out.WriteString("    length(Prefix, I),\n")
		out.WriteString("    append(Prefix, Rest, List),\n")
		out.WriteString("    Len is J - I,\n")
		out.WriteString("    length(Out, Len),\n")
		out.WriteString("    append(Out, _, Rest).\n\n")
	}
	if c.needsContains {
		out.WriteString("contains(Container, Item, Res) :-\n")
		out.WriteString("    is_dict(Container), !, (string(Item) -> atom_string(A, Item) ; A = Item), (get_dict(A, Container, _) -> Res = true ; Res = false).\n")
		out.WriteString("contains(List, Item, Res) :-\n")
		out.WriteString("    string(List), !, (sub_string(List, _, _, _, Item) -> Res = true ; Res = false).\n")
		out.WriteString("contains(List, Item, Res) :- (member(Item, List) -> Res = true ; Res = false).\n\n")
	}
	if c.needsLenAny {
		out.WriteString("len_any(Value, Len) :-\n")
		out.WriteString("    string(Value), !, string_length(Value, Len).\n")
		out.WriteString("len_any(Value, Len) :-\n")
		out.WriteString("    is_dict(Value), !, dict_pairs(Value, _, Pairs), length(Pairs, Len).\n")
		out.WriteString("len_any(Value, Len) :- length(Value, Len).\n\n")
	}
	if c.needsSetItem {
		out.WriteString("set_item(Container, Key, Val, Out) :-\n")
		out.WriteString("    is_dict(Container), !, (string(Key) -> atom_string(A, Key) ; A = Key), put_dict(A, Container, Val, Out).\n")
		out.WriteString("set_item(List, Index, Val, Out) :-\n")
		out.WriteString("    nth0(Index, List, _, Rest),\n")
		out.WriteString("    nth0(Index, Out, Val, Rest).\n\n")
	}
	out.Write(c.lambdaBuf.Bytes())
	out.Write(c.funcBuf.Bytes())
	out.Write(c.buf.Bytes())
	return out.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	oldVars := c.vars
	oldTmp := c.tmp
	c.vars = make(map[string]string)
	c.tmp = 0

	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		name := sanitizeVar(p.Name)
		params[i] = name
		c.vars[p.Name] = name
	}
	resVar := "_Res"
	c.retVar = resVar
	c.writeln(fmt.Sprintf("%s(%s, %s) :-", sanitizeVar(fn.Name), strings.Join(params, ", "), resVar))
	c.indent++
	for i, st := range fn.Body {
		if i == len(fn.Body)-1 && st.Return != nil {
			val, arith, err := c.compileExpr(st.Return.Value)
			if err != nil {
				return err
			}
			if arith {
				c.writeln(fmt.Sprintf("%s is %s.", resVar, val))
			} else {
				c.writeln(fmt.Sprintf("%s = %s.", resVar, val))
			}
		} else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
	}
	if len(fn.Body) == 0 {
		c.writeln(fmt.Sprintf("%s = true.", resVar))
	}
	c.indent--
	c.writeln("")
	c.vars = oldVars
	c.tmp = oldTmp
	c.retVar = ""
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Type != nil:
		// Type declarations are ignored in Prolog output
		return nil
	case s.Let != nil:
		if call := getSimpleCall(s.Let.Value); call != nil && call.Func == "exists" && len(call.Args) == 1 {
			if q := getQuery(call.Args[0]); q != nil {
				if err := c.compileExists(s.Let.Name, q); err != nil {
					return err
				}
				return nil
			}
		}
		val, arith, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		name := sanitizeVar(s.Let.Name)
		if arith {
			c.writeln(fmt.Sprintf("%s is %s,", name, val))
		} else {
			c.writeln(fmt.Sprintf("%s = %s,", name, val))
		}
		c.vars[s.Let.Name] = name
	case s.Var != nil:
		name := c.newVar(s.Var.Name)
		if s.Var.Value == nil {
			c.writeln(fmt.Sprintf("%s = _,", name))
			return nil
		}
		if call := getSimpleCall(s.Var.Value); call != nil && call.Func == "exists" && len(call.Args) == 1 {
			if q := getQuery(call.Args[0]); q != nil {
				if err := c.compileExists(s.Var.Name, q); err != nil {
					return err
				}
				return nil
			}
		}
		val, arith, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
		}
		if arith {
			c.writeln(fmt.Sprintf("%s is %s,", name, val))
		} else {
			c.writeln(fmt.Sprintf("%s = %s,", name, val))
		}
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			name := c.newVar(s.Assign.Name)
			val, arith, err := c.compileExpr(s.Assign.Value)
			if err != nil {
				return err
			}
			if arith {
				c.writeln(fmt.Sprintf("%s is %s,", name, val))
			} else {
				c.writeln(fmt.Sprintf("%s = %s,", name, val))
			}
		} else {
			container := c.lookupVar(s.Assign.Name)
			idxVals := make([]string, 0, len(s.Assign.Index)+len(s.Assign.Field))
			for _, idx := range s.Assign.Index {
				if idx.Colon != nil {
					return fmt.Errorf("slice assignment not supported")
				}
				iv, _, err := c.compileExpr(idx.Start)
				if err != nil {
					return err
				}
				idxVals = append(idxVals, iv)
			}
			for _, f := range s.Assign.Field {
				idxVals = append(idxVals, fmt.Sprintf("'%s'", strings.ToLower(f.Name)))
			}
			val, _, err := c.compileExpr(s.Assign.Value)
			if err != nil {
				return err
			}
			containers := []string{container}
			cur := container
			for i := 0; i < len(idxVals)-1; i++ {
				tmp := c.newTmp()
				c.needsGetItem = true
				c.writeln(fmt.Sprintf("get_item(%s, %s, %s),", cur, idxVals[i], tmp))
				containers = append(containers, tmp)
				cur = tmp
			}
			tmp := c.newTmp()
			c.needsSetItem = true
			c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", cur, idxVals[len(idxVals)-1], val, tmp))
			newVal := tmp
			for i := len(idxVals) - 2; i >= 0; i-- {
				tmp2 := c.newTmp()
				c.needsSetItem = true
				c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", containers[i], idxVals[i], newVal, tmp2))
				newVal = tmp2
			}
			name := c.newVar(s.Assign.Name)
			c.writeln(fmt.Sprintf("%s = %s,", name, newVal))
		}
	case s.Return != nil:
		if c.retVar == "" {
			return fmt.Errorf("return outside function")
		}
		val, arith, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		if arith {
			c.writeln(fmt.Sprintf("%s is %s.", c.retVar, val))
		} else {
			c.writeln(fmt.Sprintf("%s = %s.", c.retVar, val))
		}
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.writeln("throw(break),")
		return nil
	case s.Continue != nil:
		c.writeln("throw(continue),")
		return nil
	case s.Expr != nil:
		if call := getPrintCall(s.Expr.Expr); call != nil {
			for i, a := range call.Args {
				if i > 0 {
					c.writeln("write(' '),")
				}
				val, arith, err := c.compileExpr(a)
				if err != nil {
					return err
				}
				if arith {
					tmp := c.newTmp()
					c.writeln(fmt.Sprintf("%s is %s,", tmp, val))
					val = tmp
				} else if isBoolExpr(val) {
					tmp := c.newTmp()
					c.writeln(fmt.Sprintf("(%s -> %s = true ; %s = false),", val, tmp, tmp))
					val = tmp
				}
				c.writeln(fmt.Sprintf("write(%s),", val))
			}
			c.writeln("nl,")
			return nil
		}
		if call := getSimpleCall(s.Expr.Expr); call != nil {
			val, _, err := c.compileExpr(s.Expr.Expr)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s,", val))
			return nil
		}
		return fmt.Errorf("unsupported expression statement")
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileIf(is *parser.IfStmt) error {
	if is.ElseIf != nil {
		return fmt.Errorf("else-if not supported")
	}
	cond, _, err := c.compileExpr(is.Cond)
	if err != nil {
		return err
	}
	if !isBoolExpr(cond) {
		cond = fmt.Sprintf("%s \\= nil", cond)
	}
	c.writeln(fmt.Sprintf("(%s ->", cond))
	c.indent++
	for _, st := range is.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if len(is.Else) > 0 {
		c.writeln(";")
		c.indent++
		for _, st := range is.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	} else {
		c.writeln("; true")
	}
	c.writeln("),")
	return nil
}

func (c *Compiler) compileFor(fs *parser.ForStmt) error {
	v := sanitizeVar(fs.Name)
	c.writeln("catch(")
	c.indent++
	c.writeln("(")
	c.indent++
	if fs.RangeEnd != nil {
		start, _, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		end, _, err := c.compileExpr(fs.RangeEnd)
		if err != nil {
			return err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("%s is %s - 1,", tmp, end))
		c.writeln(fmt.Sprintf("between(%s, %s, %s),", start, tmp, v))
	} else {
		src, _, err := c.compileExpr(fs.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("member(%s, %s),", v, src))
	}
	c.indent++
	c.writeln("catch(")
	c.indent++
	c.writeln("(")
	c.indent++
	for _, st := range fs.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("true")
	c.indent--
	c.writeln("), continue, true),")
	c.writeln("fail")
	c.indent--
	c.writeln("; true")
	c.indent--
	c.writeln("), break, true),")
	return nil
}

func (c *Compiler) compileWhile(ws *parser.WhileStmt) error {
	c.writeln("catch(")
	c.indent++
	c.writeln("(")
	c.indent++
	c.writeln("repeat,")
	cond, _, err := c.compileExpr(ws.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(%s ->", cond))
	c.indent++
	c.writeln("catch(")
	c.indent++
	c.writeln("(")
	c.indent++
	for _, st := range ws.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("true")
	c.indent--
	c.writeln("), continue, true),")
	c.writeln("fail")
	c.indent--
	c.writeln("; true)")
	c.indent--
	c.writeln("), break, true),")
	c.indent--
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, bool, error) {
	if e == nil {
		return "0", true, nil
	}
	res, arith, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", false, err
	}
	for _, op := range e.Binary.Right {
		rhs, ar, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", false, err
		}
		arith = true
		concat := false
		if op.Op == "+" && (strings.HasPrefix(res, "\"") || strings.HasPrefix(rhs, "\"")) {
			tmp := c.newTmp()
			c.writeln(fmt.Sprintf("string_concat(%s, %s, %s),", res, rhs, tmp))
			res = tmp
			arith = false
			concat = true
		} else if op.Op == "+" || op.Op == "-" || op.Op == "*" || op.Op == "/" {
			res = fmt.Sprintf("(%s %s %s)", res, op.Op, rhs)
		} else if op.Op == "%" {
			res = fmt.Sprintf("(%s mod %s)", res, rhs)
		} else if op.Op == "==" {
			res = fmt.Sprintf("(%s == %s)", res, rhs)
			arith = false
		} else if op.Op == "!=" {
			res = fmt.Sprintf("(%s \\== %s)", res, rhs)
			arith = false
		} else if op.Op == "<" || op.Op == "<=" || op.Op == ">" || op.Op == ">=" {
			res = fmt.Sprintf("(%s %s %s)", res, op.Op, rhs)
			arith = false
		} else if op.Op == "&&" {
			res = fmt.Sprintf("(%s, %s)", res, rhs)
			arith = false
		} else if op.Op == "||" {
			res = fmt.Sprintf("(%s ; %s)", res, rhs)
			arith = false
		} else if op.Op == "in" {
			tmp := c.newTmp()
			c.needsContains = true
			c.writeln(fmt.Sprintf("contains(%s, %s, %s),", rhs, res, tmp))
			res = tmp
			arith = false
		} else {
			return "", false, fmt.Errorf("unsupported op")
		}
		if !concat && ar == false && (op.Op == "+" || op.Op == "-" || op.Op == "*" || op.Op == "/" || op.Op == "%") {
			arith = true
		}
	}
	return res, arith, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, bool, error) {
	val, arith, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", false, err
	}
	for _, op := range u.Ops {
		if op == "-" {
			val = fmt.Sprintf("(-%s)", val)
			arith = true
		} else if op == "!" {
			tmp := c.newTmp()
			c.writeln(fmt.Sprintf("(%s -> %s = false ; %s = true),", val, tmp, tmp))
			val = tmp
			arith = false
		} else {
			return "", false, fmt.Errorf("unsupported unary op")
		}
	}
	return val, arith, nil
}

func (c *Compiler) compilePostfix(pf *parser.PostfixExpr) (string, bool, error) {
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		container := c.lookupVar(pf.Target.Selector.Root)
		val, arith, err := c.compileMethodCall(container, pf.Target.Selector.Tail[0], pf.Ops[0].Call)
		if err != nil {
			return "", false, err
		}
		if len(pf.Ops) > 1 {
			return "", false, fmt.Errorf("postfix not supported")
		}
		return val, arith, nil
	}
	val, arith, err := c.compilePrimary(pf.Target)
	if err != nil {
		return "", false, err
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		if op.Index != nil {
			val, arith, err = c.compileIndex(val, op.Index)
			if err != nil {
				return "", false, err
			}
			continue
		}
		if op.Field != nil {
			if i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil {
				val, arith, err = c.compileMethodCall(val, op.Field.Name, pf.Ops[i+1].Call)
				if err != nil {
					return "", false, err
				}
				i++
				continue
			}
			tmp := c.newTmp()
			c.needsGetItem = true
			c.writeln(fmt.Sprintf("get_item(%s, '%s', %s),", val, strings.ToLower(op.Field.Name), tmp))
			val = tmp
			arith = false
			continue
		}
		if op.Cast != nil {
			val, arith, err = c.compileCast(val, op.Cast)
			if err != nil {
				return "", false, err
			}
			continue
		}
		return "", false, fmt.Errorf("postfix not supported")
	}
	return val, arith, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, bool, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if v, ok := c.vars[p.Selector.Root]; ok {
			return v, false, nil
		}
		return sanitizeVar(p.Selector.Root), false, nil
	case p.Selector != nil:
		var val string
		if v, ok := c.vars[p.Selector.Root]; ok {
			val = v
		} else {
			val = sanitizeVar(p.Selector.Root)
		}
		for _, f := range p.Selector.Tail {
			tmp := c.newTmp()
			c.needsGetItem = true
			c.writeln(fmt.Sprintf("get_item(%s, '%s', %s),", val, strings.ToLower(f), tmp))
			val = tmp
		}
		return val, false, nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.List != nil:
		elems := []string{}
		for _, e := range p.List.Elems {
			s, _, err := c.compileExpr(e)
			if err != nil {
				return "", false, err
			}
			elems = append(elems, s)
		}
		return "[" + strings.Join(elems, ", ") + "]", false, nil
	case p.Struct != nil:
		pairs := []string{}
		for _, f := range p.Struct.Fields {
			val, _, err := c.compileExpr(f.Value)
			if err != nil {
				return "", false, err
			}
			key := "'" + f.Name + "'"
			pairs = append(pairs, fmt.Sprintf("%s-%s", key, val))
		}
		tmp := c.newTmp()
		tag := "p_" + strings.ToLower(p.Struct.Name)
		c.writeln(fmt.Sprintf("dict_create(%s, %s, [%s]),", tmp, tag, strings.Join(pairs, ", ")))
		return tmp, false, nil
	case p.Map != nil:
		pairs := []string{}
		for _, it := range p.Map.Items {
			if k, ok := simpleStringKey(it.Key); ok {
				val, _, err := c.compileExpr(it.Value)
				if err != nil {
					return "", false, err
				}
				pairs = append(pairs, fmt.Sprintf("%s-%s", strings.ToLower(k), val))
				continue
			}
			key, _, err := c.compileExpr(it.Key)
			if err != nil {
				return "", false, err
			}
			if strings.HasPrefix(key, "\"") && strings.HasSuffix(key, "\"") {
				key = "'" + strings.Trim(key, "\"") + "'"
			}
			val, _, err := c.compileExpr(it.Value)
			if err != nil {
				return "", false, err
			}
			pairs = append(pairs, fmt.Sprintf("%s-%s", key, val))
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("dict_create(%s, map, [%s]),", tmp, strings.Join(pairs, ", ")))
		return tmp, false, nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Query != nil:
		return c.compileQuery(p.Query)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	}
	return "", false, fmt.Errorf("unsupported primary")
}

func (c *Compiler) compileIfExpr(ix *parser.IfExpr) (string, bool, error) {
	cond, _, err := c.compileExpr(ix.Cond)
	if err != nil {
		return "", false, err
	}
	thenV, thenA, err := c.compileExpr(ix.Then)
	if err != nil {
		return "", false, err
	}
	var elseV string
	var elseA bool
	if ix.ElseIf != nil {
		elseV, elseA, err = c.compileIfExpr(ix.ElseIf)
		if err != nil {
			return "", false, err
		}
	} else if ix.Else != nil {
		elseV, elseA, err = c.compileExpr(ix.Else)
		if err != nil {
			return "", false, err
		}
	} else {
		elseV = "true"
	}
	ar := thenA && elseA
	return fmt.Sprintf("(%s -> %s ; %s)", cond, thenV, elseV), ar, nil
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, bool, error) {
	switch call.Func {
	case "append":
		if len(call.Args) != 2 {
			return "", false, fmt.Errorf("append expects 2 args")
		}
		list, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		elem, _, err := c.compileExpr(call.Args[1])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("append(%s, [%s], %s),", list, elem, tmp))
		return tmp, false, nil
	case "avg":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("avg expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		s := c.newTmp()
		n := c.newTmp()
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("sum_list(%s, %s),", arg, s))
		c.writeln(fmt.Sprintf("length(%s, %s),", arg, n))
		c.writeln(fmt.Sprintf("%s > 0,", n))
		c.writeln(fmt.Sprintf("%s is %s / %s,", tmp, s, n))
		return tmp, true, nil
	case "sum":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("sum expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("sum_list(%s, %s),", arg, tmp))
		return tmp, true, nil
	case "min":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("min expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("min_list(%s, %s),", arg, tmp))
		return tmp, true, nil
	case "max":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("max expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("max_list(%s, %s),", arg, tmp))
		return tmp, true, nil
	case "len":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("len expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.needsLenAny = true
		c.writeln(fmt.Sprintf("len_any(%s, %s),", arg, tmp))
		return tmp, true, nil
	case "count":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("count expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("length(%s, %s),", arg, tmp))
		return tmp, true, nil
	default:
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			s, _, err := c.compileExpr(a)
			if err != nil {
				return "", false, err
			}
			args[i] = s
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("%s(%s, %s),", sanitizeVar(call.Func), strings.Join(args, ", "), tmp))
		return tmp, false, nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, bool, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), true, nil
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float), true, nil
	case l.Str != nil:
		return fmt.Sprintf("\"%s\"", *l.Str), false, nil
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true", false, nil
		}
		return "false", false, nil
	default:
		return "", false, fmt.Errorf("unsupported literal")
	}
}

func (c *Compiler) lookupVar(name string) string {
	if v, ok := c.vars[name]; ok {
		return v
	}
	return sanitizeVar(name)
}

func (c *Compiler) newTmp() string {
	s := fmt.Sprintf("_V%d", c.tmp)
	c.tmp++
	return s
}

func sanitizeVar(s string) string {
	s = strings.ReplaceAll(s, "-", "_")
	if s == "" {
		return "_"
	}
	if s[0] >= 'a' && s[0] <= 'z' {
		s = strings.ToUpper(s[:1]) + s[1:]
	}
	return s
}

func simpleStringKey(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

func getPrintCall(e *parser.Expr) *parser.CallExpr {
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
	if p.Target != nil && p.Target.Call != nil && p.Target.Call.Func == "print" {
		return p.Target.Call
	}
	return nil
}

func isBoolExpr(s string) bool {
	ops := []string{"=:=", "=\\=", "<", "<=", ">", ">=", "==", "\\=="}
	for _, op := range ops {
		if strings.Contains(s, op) {
			return true
		}
	}
	return false
}

func getSimpleCall(e *parser.Expr) *parser.CallExpr {
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
	if p.Target != nil && p.Target.Call != nil {
		return p.Target.Call
	}
	return nil
}

func getQuery(e *parser.Expr) *parser.QueryExpr {
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
	return p.Target.Query
}

func (c *Compiler) compileExists(name string, q *parser.QueryExpr) error {
	src, _, err := c.compileExpr(q.Source)
	if err != nil {
		return err
	}
	varName := sanitizeVar(q.Var)
	old := c.vars
	c.vars = make(map[string]string)
	for k, v := range old {
		c.vars[k] = v
	}
	c.vars[q.Var] = varName
	cond := "true"
	if q.Where != nil {
		cond, _, err = c.compileExpr(q.Where)
		if err != nil {
			c.vars = old
			return err
		}
	}
	c.vars = old
	target := sanitizeVar(name)
	c.writeln(fmt.Sprintf("(once((member(%s, %s), %s)) -> %s = true ; %s = false),", varName, src, cond, target, target))
	c.vars[name] = target
	return nil
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, bool, error) {
	oldVars := c.vars
	c.vars = make(map[string]string)
	for k, v := range oldVars {
		c.vars[k] = v
	}

	loops := []string{}
	varNames := []string{}
	src, _, err := c.compileExpr(q.Source)
	if err != nil {
		c.vars = oldVars
		return "", false, err
	}
	vname := sanitizeVar(q.Var)
	c.vars[q.Var] = vname
	loops = append(loops, fmt.Sprintf("member(%s, %s)", vname, src))
	varNames = append(varNames, vname)

	for _, fr := range q.Froms {
		fs, _, err := c.compileExpr(fr.Src)
		if err != nil {
			c.vars = oldVars
			return "", false, err
		}
		fname := sanitizeVar(fr.Var)
		c.vars[fr.Var] = fname
		loops = append(loops, fmt.Sprintf("member(%s, %s)", fname, fs))
		varNames = append(varNames, fname)
	}

	for _, j := range q.Joins {
		js, _, err := c.compileExpr(j.Src)
		if err != nil {
			c.vars = oldVars
			return "", false, err
		}
		jname := sanitizeVar(j.Var)
		c.vars[j.Var] = jname
		var onBuf bytes.Buffer
		oldOut := c.out
		c.out = &onBuf
		onCond, _, err := c.compileExpr(j.On)
		c.out = oldOut
		if err != nil {
			c.vars = oldVars
			return "", false, err
		}
		onLines := []string{}
		for _, line := range strings.Split(strings.TrimSpace(onBuf.String()), "\n") {
			line = strings.TrimSuffix(strings.TrimSpace(line), ",")
			if line != "" {
				onLines = append(onLines, line)
			}
		}
		oldLoopStr := strings.Join(loops, ", ")
		leftTuple := "[" + strings.Join(varNames, ", ") + "]"
		nilAssignParts := make([]string, len(varNames))
		for i, v := range varNames {
			nilAssignParts[i] = fmt.Sprintf("%s = nil", v)
		}
		nilAssign := strings.Join(nilAssignParts, ", ")
		if j.Side != nil && *j.Side == "left" {
			tmp := c.newTmp()
			joined := strings.Join(append([]string{fmt.Sprintf("member(%s, %s)", jname, js)}, onLines...), ", ")
			loops = append(loops, fmt.Sprintf("findall(%s, (%s, %s), %s)", jname, joined, onCond, tmp))
			loops = append(loops, fmt.Sprintf("(%s = [] -> %s = nil; member(%s, %s))", tmp, jname, jname, tmp))
			varNames = append(varNames, jname)
			continue
		}
		if j.Side != nil && *j.Side == "right" {
			tmp := c.newTmp()
			findBodyParts := []string{}
			if oldLoopStr != "" {
				findBodyParts = append(findBodyParts, oldLoopStr)
			}
			findBodyParts = append(findBodyParts, onLines...)
			findBodyParts = append(findBodyParts, onCond)
			findBody := strings.Join(findBodyParts, ", ")
			loops = []string{
				fmt.Sprintf("member(%s, %s)", jname, js),
				fmt.Sprintf("findall(%s, (%s), %s)", leftTuple, findBody, tmp),
				fmt.Sprintf("(%s = [] -> (%s) ; member(%s, %s))", tmp, nilAssign, leftTuple, tmp),
			}
			varNames = append(varNames, jname)
			continue
		}
		if j.Side != nil && *j.Side == "outer" {
			tmpL := c.newTmp()
			joined := strings.Join(append([]string{fmt.Sprintf("member(%s, %s)", jname, js)}, onLines...), ", ")
			leftLoops := append(append([]string{}, loops...), fmt.Sprintf("findall(%s, (%s, %s), %s)", jname, joined, onCond, tmpL), fmt.Sprintf("(%s = [] -> %s = nil; member(%s, %s))", tmpL, jname, jname, tmpL))
			leftPart := strings.Join(leftLoops, ", ")

			tmpR := c.newTmp()
			findBodyParts := []string{}
			if oldLoopStr != "" {
				findBodyParts = append(findBodyParts, oldLoopStr)
			}
			findBodyParts = append(findBodyParts, onLines...)
			findBodyParts = append(findBodyParts, onCond)
			findBodyR := strings.Join(findBodyParts, ", ")
			rightLoops := []string{
				fmt.Sprintf("member(%s, %s)", jname, js),
				fmt.Sprintf("findall(%s, (%s), %s)", leftTuple, findBodyR, tmpR),
				fmt.Sprintf("%s = []", tmpR),
				nilAssign,
			}
			rightPart := strings.Join(rightLoops, ", ")
			loops = []string{fmt.Sprintf("((%s);(%s))", leftPart, rightPart)}
			varNames = append(varNames, jname)
			continue
		}

		loops = append(loops, fmt.Sprintf("member(%s, %s)", jname, js))
		loops = append(loops, onLines...)
		loops = append(loops, onCond)
		varNames = append(varNames, jname)
	}

	cond := "true"
	if q.Where != nil {
		var whereBuf bytes.Buffer
		oldOut2 := c.out
		c.out = &whereBuf
		ccond, _, err := c.compileExpr(q.Where)
		c.out = oldOut2
		if err != nil {
			c.vars = oldVars
			return "", false, err
		}
		for _, line := range strings.Split(strings.TrimSpace(whereBuf.String()), "\n") {
			line = strings.TrimSuffix(strings.TrimSpace(line), ",")
			if line != "" {
				loops = append(loops, line)
			}
		}
		cond = ccond
	}
	loops = append(loops, cond)

	var selBuf bytes.Buffer
	oldOut := c.out
	c.out = &selBuf
	selVal, _, err := c.compileExpr(q.Select)
	c.out = oldOut
	if err != nil {
		c.vars = oldVars
		return "", false, err
	}
	selLines := []string{}
	for _, line := range strings.Split(strings.TrimSpace(selBuf.String()), "\n") {
		line = strings.TrimSuffix(strings.TrimSpace(line), ",")
		if line != "" {
			selLines = append(selLines, line)
		}
	}
	resVar := c.newTmp()
	loops = append(loops, selLines...)
	loops = append(loops, fmt.Sprintf("%s = %s", resVar, selVal))
	tmp := c.newTmp()
	c.writeln(fmt.Sprintf("findall(%s, (%s), %s),", resVar, strings.Join(loops, ", "), tmp))
	c.vars = oldVars
	return tmp, false, nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, bool, error) {
	name := fmt.Sprintf("p__lambda%d", c.tmp)
	c.tmp++
	var body []*parser.Statement
	if fn.ExprBody != nil {
		body = []*parser.Statement{{Return: &parser.ReturnStmt{Value: fn.ExprBody}}}
	} else {
		body = fn.BlockBody
	}
	fs := &parser.FunStmt{Name: name, Params: fn.Params, Body: body}
	oldIndent := c.indent
	oldOut := c.out
	c.indent = 0
	c.out = &c.lambdaBuf
	if err := c.compileFun(fs); err != nil {
		c.indent = oldIndent
		c.out = oldOut
		return "", false, err
	}
	c.indent = oldIndent
	c.out = oldOut
	return name, false, nil
}

func (c *Compiler) compileIndex(container string, idx *parser.IndexOp) (string, bool, error) {
	if idx.Colon == nil {
		index, _, err := c.compileExpr(idx.Start)
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.needsGetItem = true
		c.writeln(fmt.Sprintf("get_item(%s, %s, %s),", container, index, tmp))
		return tmp, false, nil
	}
	start := "0"
	if idx.Start != nil {
		s, _, err := c.compileExpr(idx.Start)
		if err != nil {
			return "", false, err
		}
		start = s
	}
	end := "0"
	if idx.End != nil {
		e, _, err := c.compileExpr(idx.End)
		if err != nil {
			return "", false, err
		}
		end = e
	}
	tmp := c.newTmp()
	c.needsSlice = true
	c.writeln(fmt.Sprintf("slice(%s, %s, %s, %s),", container, start, end, tmp))
	return tmp, false, nil
}

func (c *Compiler) compileMethodCall(container, method string, call *parser.CallOp) (string, bool, error) {
	if method == "contains" && len(call.Args) == 1 {
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.needsContains = true
		c.writeln(fmt.Sprintf("contains(%s, %s, %s),", container, arg, tmp))
		return tmp, false, nil
	}
	return "", false, fmt.Errorf("unsupported method")
}

func (c *Compiler) compileCast(val string, cast *parser.CastOp) (string, bool, error) {
	if cast.Type != nil && cast.Type.Simple != nil {
		switch *cast.Type.Simple {
		case "int":
			tmp := c.newTmp()
			c.writeln(fmt.Sprintf("number_string(%s, %s),", tmp, val))
			return tmp, true, nil
		default:
			return val, false, nil
		}
	}
	return val, false, nil
}
