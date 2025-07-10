//go:build slow

package pl

import (
	"bytes"
	"fmt"
	"sort"
	"strings"
	"unicode"

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
	mutVars       map[string]bool
	retVar        string
	needsGetItem  bool
	needsSlice    bool
	needsContains bool
	needsLenAny   bool
	needsSetItem  bool
	needsGroup    bool
	needsSetOps   bool
	needsLoad     bool
	needsSave     bool
	needsExpect   bool

	currentFun string
	nested     map[string]nestedInfo
}

type nestedInfo struct {
	name     string
	captured []string
}

func New() *Compiler {
	c := &Compiler{vars: make(map[string]string), mutVars: make(map[string]bool)}
	c.nested = make(map[string]nestedInfo)
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
	c.mutVars = make(map[string]bool)
	c.needsGetItem = false
	c.needsSlice = false
	c.needsContains = false
	c.needsLenAny = false
	c.needsSetItem = false
	c.needsGroup = false
	c.needsSetOps = false
	c.needsLoad = false
	c.needsSave = false
	c.needsExpect = false
	c.nested = make(map[string]nestedInfo)
	c.currentFun = ""

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
	if c.needsGroup {
		out.WriteString("to_list(Str, L) :-\n")
		out.WriteString("    string(Str), !,\n")
		out.WriteString("    string_chars(Str, L).\n")
		out.WriteString("to_list(L, L).\n\n")

		out.WriteString("count(V, R) :-\n")
		out.WriteString("    is_dict(V), !, get_dict('Items', V, Items), length(Items, R).\n")
		out.WriteString("count(V, R) :-\n")
		out.WriteString("    string(V), !, string_chars(V, C), length(C, R).\n")
		out.WriteString("count(V, R) :-\n")
		out.WriteString("    is_list(V), !, length(V, R).\n")
		out.WriteString("count(_, _) :- throw(error('count expects list or group')).\n\n")

		out.WriteString("avg(V, R) :-\n")
		out.WriteString("    is_dict(V), !, get_dict('Items', V, Items), avg_list(Items, R).\n")
		out.WriteString("avg(V, R) :-\n")
		out.WriteString("    is_list(V), !, avg_list(V, R).\n")
		out.WriteString("avg(_, _) :- throw(error('avg expects list or group')).\n")
		out.WriteString("avg_list([], 0).\n")
		out.WriteString("avg_list(L, R) :- sum_list(L, S), length(L, N), N > 0, R is S / N.\n\n")

		out.WriteString("sum(V, R) :-\n")
		out.WriteString("    is_dict(V), !, get_dict('Items', V, Items), sum_list(Items, R).\n")
		out.WriteString("sum(V, R) :-\n")
		out.WriteString("    is_list(V), !, sum_list(V, R).\n")
		out.WriteString("sum(_, _) :- throw(error('sum expects list or group')).\n\n")

		out.WriteString("group_insert(Key, Item, [], [_{key:Key, 'Items':[Item]}]).\n")
		out.WriteString("group_insert(Key, Item, [G|Gs], [NG|Gs]) :- get_dict(key, G, Key), !, get_dict('Items', G, Items), append(Items, [Item], NItems), put_dict('Items', G, NItems, NG).\n")
		out.WriteString("group_insert(Key, Item, [G|Gs], [G|Rs]) :- group_insert(Key, Item, Gs, Rs).\n")
		out.WriteString("group_pairs([], Acc, Res) :- reverse(Acc, Res).\n")
		out.WriteString("group_pairs([K-V|T], Acc, Res) :- group_insert(K, V, Acc, Acc1), group_pairs(T, Acc1, Res).\n")
		out.WriteString("group_by(List, Fn, Groups) :- findall(K-V, (member(V, List), call(Fn, V, K)), Pairs), group_pairs(Pairs, [], Groups).\n\n")
	}
	if c.needsSetOps {
		out.WriteString("union(A, B, R) :- append(A, B, C), list_to_set(C, R).\n")
		out.WriteString("except([], _, []).\n")
		out.WriteString("except([H|T], B, R) :- memberchk(H, B), !, except(T, B, R).\n")
		out.WriteString("except([H|T], B, [H|R]) :- except(T, B, R).\n\n")
		out.WriteString("intersect(A, B, R) :- intersect(A, B, [], R).\n")
		out.WriteString("intersect([], _, Acc, R) :- reverse(Acc, R).\n")
		out.WriteString("intersect([H|T], B, Acc, R) :- memberchk(H, B), \\+ memberchk(H, Acc), !, intersect(T, B, [H|Acc], R).\n")
		out.WriteString("intersect([_|T], B, Acc, R) :- intersect(T, B, Acc, R).\n\n")
	}
	if c.needsLoad || c.needsSave {
		out.WriteString(":- use_module(library(http/json)).\n")
	}
	if c.needsLoad {
		out.WriteString("load_data(Path, Opts, Rows) :-\n")
		out.WriteString("    (is_dict(Opts), get_dict(format, Opts, Fmt) -> true ; Fmt = 'json'),\n")
		out.WriteString("    (Path == '' ; Path == '-' -> read_string(user_input, _, Text) ; read_file_to_string(Path, Text, [])),\n")
		out.WriteString("    (Fmt == 'jsonl' ->\n")
		out.WriteString("        split_string(Text, '\\n', ' \\t\\r', Lines0),\n")
		out.WriteString("        exclude(=(''), Lines0, Lines),\n")
		out.WriteString("        findall(D, (member(L, Lines), open_string(L, S), json_read_dict(S, D), close(S)), Rows)\n")
		out.WriteString("    ;\n")
		out.WriteString("        open_string(Text, S), json_read_dict(S, Data), close(S),\n")
		out.WriteString("        (is_list(Data) -> Rows = Data ; Rows = [Data])\n")
		out.WriteString("    ).\n\n")
	}
	if c.needsSave {
		out.WriteString("save_data(Rows, Path, Opts) :-\n")
		out.WriteString("    (is_dict(Opts), get_dict(format, Opts, Fmt) -> true ; Fmt = 'json'),\n")
		out.WriteString("    (Path == '' ; Path == '-' -> Out = current_output ; open(Path, write, Out)),\n")
		out.WriteString("    (Fmt == 'jsonl' ->\n")
		out.WriteString("        forall(member(R, Rows), (json_write_dict(Out, R), nl(Out)))\n")
		out.WriteString("    ;\n")
		out.WriteString("        json_write_dict(Out, Rows)\n")
		out.WriteString("    ),\n")
		out.WriteString("    (Out == current_output -> flush_output(Out) ; close(Out)).\n\n")
	}
	if c.needsExpect {
		out.WriteString("expect(Cond) :- (Cond -> true ; throw(error('expect failed'))).\n\n")
	}
	out.Write(c.lambdaBuf.Bytes())
	out.Write(c.funcBuf.Bytes())
	out.Write(c.buf.Bytes())
	return out.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	oldVars := c.vars
	oldTmp := c.tmp
	oldNested := c.nested
	oldCurrent := c.currentFun

	c.vars = make(map[string]string)
	c.tmp = 0
	c.nested = make(map[string]nestedInfo)
	c.currentFun = sanitizeAtom(fn.Name)

	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		name := sanitizeVar(p.Name)
		params[i] = name
		c.vars[p.Name] = name
	}
	resVar := "_Res"
	c.retVar = resVar
	// compile nested functions before the outer body
	for _, st := range fn.Body {
		if st.Fun != nil {
			if err := c.compileNestedFun(st.Fun); err != nil {
				return err
			}
		}
	}
	c.writeln(fmt.Sprintf("%s(%s, %s) :-", sanitizeAtom(fn.Name), strings.Join(params, ", "), resVar))
	c.indent++
	for i, st := range fn.Body {
		if st.Fun != nil {
			continue
		}
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
	c.nested = oldNested
	c.currentFun = oldCurrent
	c.retVar = ""
	return nil
}

func (c *Compiler) compileNestedFun(fn *parser.FunStmt) error {
	keys := make([]string, 0, len(c.vars))
	for k := range c.vars {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	captures := make([]string, len(keys))
	params := make([]*parser.Param, 0, len(keys)+len(fn.Params))
	for i, k := range keys {
		captures[i] = k
		params = append(params, &parser.Param{Name: sanitizeVar(k)})
	}
	params = append(params, fn.Params...)
	name := fmt.Sprintf("%s__%s", c.currentFun, fn.Name)
	newFn := &parser.FunStmt{Name: name, Params: params, Body: fn.Body}
	c.nested[fn.Name] = nestedInfo{name: sanitizeAtom(name), captured: captures}
	err := c.compileFun(newFn)
	delete(c.nested, fn.Name)
	return err
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
		name := sanitizeAtom(s.Var.Name)
		c.mutVars[s.Var.Name] = true
		c.vars[s.Var.Name] = name
		if s.Var.Value == nil {
			c.writeln(fmt.Sprintf("nb_setval(%s, _),", name))
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
			tmp := c.newTmp()
			c.writeln(fmt.Sprintf("%s is %s,", tmp, val))
			val = tmp
		}
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", name, val))
	case s.Assign != nil:
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			val, arith, err := c.compileExpr(s.Assign.Value)
			if err != nil {
				return err
			}
			if c.mutVars[s.Assign.Name] {
				if arith {
					tmp := c.newTmp()
					c.writeln(fmt.Sprintf("%s is %s,", tmp, val))
					val = tmp
				}
				c.writeln(fmt.Sprintf("nb_setval(%s, %s),", sanitizeAtom(s.Assign.Name), val))
			} else {
				name := c.newVar(s.Assign.Name)
				if arith {
					c.writeln(fmt.Sprintf("%s is %s,", name, val))
				} else {
					c.writeln(fmt.Sprintf("%s = %s,", name, val))
				}
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
			if c.mutVars[s.Assign.Name] {
				c.writeln(fmt.Sprintf("nb_setval(%s, %s),", sanitizeAtom(s.Assign.Name), newVal))
			} else {
				name := c.newVar(s.Assign.Name)
				c.writeln(fmt.Sprintf("%s = %s,", name, newVal))
			}
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
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
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
		if se := getSaveExpr(s.Expr.Expr); se != nil {
			val, _, err := c.compileExpr(s.Expr.Expr)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("%s,", val))
			return nil
		}
		return fmt.Errorf("unsupported expression statement")
	case s.Fun != nil:
		// Nested functions are compiled separately
		return nil
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
	c.writeln("true")
	c.indent--
	if len(is.Else) > 0 {
		c.writeln(";")
		c.indent++
		for _, st := range is.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.writeln("true")
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

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	val, arith, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	if arith {
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("%s is %s,", tmp, val))
		val = tmp
	}
	c.writeln(fmt.Sprintf("expect(%s),", val))
	c.needsExpect = true
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := sanitizeAtom("test_" + t.Name)
	oldOut := c.out
	oldVars := c.vars
	oldTmp := c.tmp
	c.out = &c.funcBuf
	c.vars = make(map[string]string)
	c.tmp = 0
	c.writeln(fmt.Sprintf("%s :-", name))
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			c.out = oldOut
			c.vars = oldVars
			c.tmp = oldTmp
			return err
		}
	}
	c.writeln("true.")
	c.indent--
	c.writeln("")
	c.out = oldOut
	c.vars = oldVars
	c.tmp = oldTmp
	c.writeln(fmt.Sprintf("%s,", name))
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	listVar := c.lookupVar(u.Target)
	helper := sanitizeAtom(u.Target + "_update")

	// collect identifiers used
	used := map[string]struct{}{}
	if u.Where != nil {
		collectIdents(u.Where, used)
	}
	for _, it := range u.Set.Items {
		if k, ok := identName(it.Key); ok {
			used[k] = struct{}{}
		}
		collectIdents(it.Value, used)
	}

	// define helper predicate
	oldOut := c.out
	c.out = &c.funcBuf
	itemVar := c.newTmp()
	restVar := c.newTmp()
	newItem := c.newTmp()
	restOut := c.newTmp()
	c.writeln(fmt.Sprintf("%s([], []).", helper))
	c.writeln(fmt.Sprintf("%s([%s|%s], [%s|%s]) :-", helper, itemVar, restVar, newItem, restOut))
	c.indent++
	oldVars := c.vars
	c.vars = make(map[string]string)
	for k, v := range oldVars {
		c.vars[k] = v
	}
	for f := range used {
		fv := sanitizeVar(f)
		c.writeln(fmt.Sprintf("get_item(%s, '%s', %s),", itemVar, strings.ToLower(f), fv))
		c.vars[f] = fv
	}
	cond := "true"
	if u.Where != nil {
		var buf bytes.Buffer
		outOld := c.out
		c.out = &buf
		cv, _, err := c.compileExpr(u.Where)
		c.out = outOld
		if err != nil {
			c.out = oldOut
			c.vars = oldVars
			return err
		}
		for _, line := range strings.Split(strings.TrimSpace(buf.String()), "\n") {
			line = strings.TrimSuffix(strings.TrimSpace(line), ",")
			if line != "" {
				c.writeln(line + ",")
			}
		}
		cond = cv
	}
	c.writeln(fmt.Sprintf("(%s ->", cond))
	c.indent++
	tmpVal := itemVar
	for _, it := range u.Set.Items {
		var valBuf bytes.Buffer
		outOld := c.out
		c.out = &valBuf
		vv, _, err := c.compileExpr(it.Value)
		c.out = outOld
		if err != nil {
			c.out = oldOut
			c.vars = oldVars
			return err
		}
		for _, line := range strings.Split(strings.TrimSpace(valBuf.String()), "\n") {
			line = strings.TrimSuffix(strings.TrimSpace(line), ",")
			if line != "" {
				c.writeln(line + ",")
			}
		}
		keyExpr := ""
		if k, ok := identName(it.Key); ok {
			keyExpr = fmt.Sprintf("'%s'", strings.ToLower(k))
		} else {
			var kbuf bytes.Buffer
			c.out = &kbuf
			kv, _, err := c.compileExpr(it.Key)
			c.out = outOld
			if err != nil {
				c.out = oldOut
				c.vars = oldVars
				return err
			}
			for _, line := range strings.Split(strings.TrimSpace(kbuf.String()), "\n") {
				line = strings.TrimSuffix(strings.TrimSpace(line), ",")
				if line != "" {
					c.writeln(line + ",")
				}
			}
			keyExpr = kv
		}
		tmp2 := c.newTmp()
		c.needsSetItem = true
		c.writeln(fmt.Sprintf("set_item(%s, %s, %s, %s),", tmpVal, keyExpr, vv, tmp2))
		tmpVal = tmp2
	}
	c.writeln(fmt.Sprintf("%s = %s", newItem, tmpVal))
	c.indent--
	c.writeln(";")
	c.indent++
	c.writeln(fmt.Sprintf("%s = %s", newItem, itemVar))
	c.indent--
	c.writeln("),")
	c.writeln(fmt.Sprintf("%s(%s, %s).", helper, restVar, restOut))
	c.indent--
	c.writeln("")
	c.vars = oldVars
	c.out = oldOut

	// call helper
	outVar := c.newTmp()
	c.writeln(fmt.Sprintf("%s(%s, %s),", helper, listVar, outVar))
	if c.mutVars[u.Target] {
		c.writeln(fmt.Sprintf("nb_setval(%s, %s),", sanitizeAtom(u.Target), outVar))
	} else {
		name := c.newVar(u.Target)
		c.writeln(fmt.Sprintf("%s = %s,", name, outVar))
	}
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
		} else if op.Op == "union" {
			tmp := c.newTmp()
			if op.All {
				c.writeln(fmt.Sprintf("append(%s, %s, %s),", res, rhs, tmp))
			} else {
				c.needsSetOps = true
				c.writeln(fmt.Sprintf("union(%s, %s, %s),", res, rhs, tmp))
			}
			res = tmp
			arith = false
		} else if op.Op == "except" {
			tmp := c.newTmp()
			c.needsSetOps = true
			c.writeln(fmt.Sprintf("except(%s, %s, %s),", res, rhs, tmp))
			res = tmp
			arith = false
		} else if op.Op == "intersect" {
			tmp := c.newTmp()
			c.needsSetOps = true
			c.writeln(fmt.Sprintf("intersect(%s, %s, %s),", res, rhs, tmp))
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
		if c.mutVars[p.Selector.Root] {
			tmp := c.newTmp()
			c.writeln(fmt.Sprintf("nb_getval(%s, %s),", sanitizeAtom(p.Selector.Root), tmp))
			return tmp, false, nil
		}
		if v, ok := c.vars[p.Selector.Root]; ok {
			return v, false, nil
		}
		if len(p.Selector.Root) > 0 && unicode.IsUpper(rune(p.Selector.Root[0])) {
			return sanitizeAtom(p.Selector.Root), false, nil
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
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Query != nil:
		return c.compileQuery(p.Query)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
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
	tmp := c.newTmp()
	c.writeln(fmt.Sprintf("(%s -> %s = %s ; %s = %s),", cond, tmp, thenV, tmp, elseV))
	return tmp, ar, nil
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, bool, error) {
	if info, ok := c.nested[call.Func]; ok {
		args := make([]string, 0, len(info.captured)+len(call.Args))
		for _, v := range info.captured {
			args = append(args, c.lookupVar(v))
		}
		for _, a := range call.Args {
			s, _, err := c.compileExpr(a)
			if err != nil {
				return "", false, err
			}
			args = append(args, s)
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("%s(%s, %s),", info.name, strings.Join(args, ", "), tmp))
		return tmp, false, nil
	}
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
		tmp := c.newTmp()
		c.needsGroup = true
		c.writeln(fmt.Sprintf("avg(%s, %s),", arg, tmp))
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
		c.needsGroup = true
		c.writeln(fmt.Sprintf("sum(%s, %s),", arg, tmp))
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
		c.needsGroup = true
		c.writeln(fmt.Sprintf("count(%s, %s),", arg, tmp))
		return tmp, true, nil
	case "str":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("str expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("term_string(%s, %s),", arg, tmp))
		return tmp, false, nil
	case "values":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("values expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		pairs := c.newTmp()
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("dict_pairs(%s, _, %s),", arg, pairs))
		c.writeln(fmt.Sprintf("findall(V, member(_-V, %s), %s),", pairs, tmp))
		return tmp, false, nil
	case "json":
		if len(call.Args) != 1 {
			return "", false, fmt.Errorf("json expects 1 arg")
		}
		arg, _, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", false, err
		}
		c.needsLoad = true
		c.writeln(fmt.Sprintf("json_write_dict(current_output, %s), nl,", arg))
		return "true", false, nil
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
		c.writeln(fmt.Sprintf("%s(%s, %s),", sanitizeAtom(call.Func), strings.Join(args, ", "), tmp))
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
	if c.mutVars[name] {
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("nb_getval(%s, %s),", sanitizeAtom(name), tmp))
		return tmp
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

func sanitizeAtom(s string) string {
	s = strings.ReplaceAll(s, "-", "_")
	if s == "" {
		return ""
	}
	if s[0] >= 'A' && s[0] <= 'Z' {
		s = strings.ToLower(s[:1]) + s[1:]
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

	if q.Group != nil {
		// compile key expression (only first expr supported)
		var keyBuf bytes.Buffer
		oldOut := c.out
		c.out = &keyBuf
		keyVal, _, err := c.compileExpr(q.Group.Exprs[0])
		c.out = oldOut
		if err != nil {
			c.vars = oldVars
			return "", false, err
		}
		for _, line := range strings.Split(strings.TrimSpace(keyBuf.String()), "\n") {
			line = strings.TrimSuffix(strings.TrimSpace(line), ",")
			if line != "" {
				loops = append(loops, line)
			}
		}
		keyVar := c.newTmp()
		loops = append(loops, fmt.Sprintf("%s = %s", keyVar, keyVal))

		itemVar := c.newTmp()
		fields := make([]string, len(varNames))
		for i, v := range varNames {
			fields[i] = fmt.Sprintf("'%s'-%s", v, v)
		}
		loops = append(loops, fmt.Sprintf("dict_create(%s, map, [%s])", itemVar, strings.Join(fields, ", ")))
		pairVar := c.newTmp()
		loops = append(loops, fmt.Sprintf("%s = %s-%s", pairVar, keyVar, itemVar))

		pairList := c.newTmp()
		c.writeln(fmt.Sprintf("findall(%s, (%s), %s),", pairVar, strings.Join(loops, ", "), pairList))
		groupsVar := c.newTmp()
		c.needsGroup = true
		c.writeln(fmt.Sprintf("group_pairs(%s, [], %s),", pairList, groupsVar))

		gname := sanitizeVar(q.Group.Name)
		c.vars = oldVars
		c.vars[q.Group.Name] = gname

		var selBuf bytes.Buffer
		oldOut2 := c.out
		c.out = &selBuf
		selVal, _, err := c.compileExpr(q.Select)
		c.out = oldOut2
		if err != nil {
			return "", false, err
		}
		selLines := []string{}
		for _, line := range strings.Split(strings.TrimSpace(selBuf.String()), "\n") {
			line = strings.TrimSuffix(strings.TrimSpace(line), ",")
			if line != "" {
				selLines = append(selLines, line)
			}
		}
		loops2 := []string{fmt.Sprintf("member(%s, %s)", gname, groupsVar)}
		loops2 = append(loops2, selLines...)
		resVar := c.newTmp()
		loops2 = append(loops2, fmt.Sprintf("%s = %s", resVar, selVal))
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("findall(%s, (%s), %s),", resVar, strings.Join(loops2, ", "), tmp))
		return tmp, false, nil
	}

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

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, bool, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "nil"
	if l.With != nil {
		v, _, err := c.compileExpr(l.With)
		if err != nil {
			return "", false, err
		}
		opts = v
	}
	tmp := c.newTmp()
	c.needsLoad = true
	c.writeln(fmt.Sprintf("load_data(%s, %s, %s),", path, opts, tmp))
	return tmp, false, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, bool, error) {
	src, _, err := c.compileExpr(s.Src)
	if err != nil {
		return "", false, err
	}
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, _, err := c.compileExpr(s.With)
		if err != nil {
			return "", false, err
		}
		opts = v
	}
	c.needsSave = true
	return fmt.Sprintf("save_data(%s, %s, %s)", src, path, opts), false, nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, bool, error) {
	target, _, err := c.compileExpr(m.Target)
	if err != nil {
		return "", false, err
	}
	var cur string
	var ar bool
	for i := len(m.Cases) - 1; i >= 0; i-- {
		cs := m.Cases[i]
		res, resAr, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", false, err
		}
		if cur == "" {
			cur = res
			ar = resAr
			continue
		}
		pat, _, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", false, err
		}
		tmp := c.newTmp()
		c.writeln(fmt.Sprintf("(%s == %s -> %s = %s ; %s = %s),", target, pat, tmp, res, tmp, cur))
		cur = tmp
		ar = ar && resAr
	}
	return cur, ar, nil
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func getSaveExpr(e *parser.Expr) *parser.SaveExpr {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Save == nil {
		return nil
	}
	return p.Target.Save
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return "", false
	}
	if len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func collectIdents(e *parser.Expr, out map[string]struct{}) {
	if e == nil || e.Binary == nil {
		return
	}
	var scanPrimary func(*parser.Primary)
	var scanPostfix func(*parser.PostfixExpr)
	var scanUnary func(*parser.Unary)

	scanUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		scanPostfix(u.Value)
	}
	scanPostfix = func(p *parser.PostfixExpr) {
		if p == nil {
			return
		}
		scanPrimary(p.Target)
		for _, op := range p.Ops {
			if op.Index != nil {
				collectIdents(op.Index.Start, out)
				collectIdents(op.Index.End, out)
			} else if op.Call != nil {
				for _, a := range op.Call.Args {
					collectIdents(a, out)
				}
			}
		}
	}
	scanPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		switch {
		case p.Query != nil:
			collectIdents(p.Query.Source, out)
			for _, f := range p.Query.Froms {
				collectIdents(f.Src, out)
			}
			for _, j := range p.Query.Joins {
				collectIdents(j.Src, out)
				collectIdents(j.On, out)
			}
			collectIdents(p.Query.Where, out)
			if p.Query.Group != nil {
				collectIdents(p.Query.Group.Exprs[0], out)
			}
			collectIdents(p.Query.Sort, out)
			collectIdents(p.Query.Skip, out)
			collectIdents(p.Query.Take, out)
			collectIdents(p.Query.Select, out)
		case p.FunExpr != nil:
			collectIdents(p.FunExpr.ExprBody, out)
		case p.If != nil:
			collectIdents(p.If.Cond, out)
			collectIdents(p.If.Then, out)
			collectIdents(p.If.Else, out)
		case p.Match != nil:
			collectIdents(p.Match.Target, out)
			for _, c := range p.Match.Cases {
				collectIdents(c.Pattern, out)
				collectIdents(c.Result, out)
			}
		case p.List != nil:
			for _, el := range p.List.Elems {
				collectIdents(el, out)
			}
		case p.Map != nil:
			for _, it := range p.Map.Items {
				collectIdents(it.Key, out)
				collectIdents(it.Value, out)
			}
		case p.Call != nil:
			for _, a := range p.Call.Args {
				collectIdents(a, out)
			}
		case p.Selector != nil:
			out[p.Selector.Root] = struct{}{}
		case p.Group != nil:
			collectIdents(p.Group, out)
		case p.Generate != nil:
			for _, f := range p.Generate.Fields {
				collectIdents(f.Value, out)
			}
		case p.Fetch != nil:
			collectIdents(p.Fetch.URL, out)
			collectIdents(p.Fetch.With, out)
		case p.Load != nil:
			collectIdents(p.Load.With, out)
		case p.Save != nil:
			collectIdents(p.Save.Src, out)
			collectIdents(p.Save.With, out)
		}
	}

	scanUnary(e.Binary.Left)
	for _, part := range e.Binary.Right {
		scanPostfix(part.Right)
	}
}
