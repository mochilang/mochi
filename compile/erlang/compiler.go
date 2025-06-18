package erlcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Erlang source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	env     *types.Env
	needGet bool
}

// New returns a new Compiler.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

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

// Compile generates Erlang source for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.writeln("#!/usr/bin/env escript")
	c.writeln("-module(main).")

	// collect exported functions
	exports := []string{"main/1"}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			exports = append(exports, fmt.Sprintf("%s/%d", s.Fun.Name, len(s.Fun.Params)))
		}
	}
	c.writeln("-export([" + strings.Join(exports, ", ") + "]).")
	c.writeln("")

	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("main(_) ->")
	c.indent++
	if err := c.compileBlock(prog.Statements, true, func(s *parser.Statement) bool { return s.Fun == nil }); err != nil {
		return nil, err
	}
	c.indent--

	c.writeln("")
	c.emitRuntime()

	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fun *parser.FunStmt) error {
	params := []string{}
	for _, p := range fun.Params {
		params = append(params, p.Name)
	}
	c.writeln(fmt.Sprintf("%s(%s) ->", fun.Name, strings.Join(params, ", ")))
	c.indent++
	c.writeln("try")
	c.indent++
	if err := c.compileBlock(fun.Body, false, nil); err != nil {
		return err
	}
	c.indent--
	c.writeln("catch")
	c.indent++
	c.writeln("throw:{return, V} -> V")
	c.indent--
	c.writeln("end.")
	c.indent--
	return nil
}

// compileBlock writes a sequence of statements separated by commas. If filter is
// provided, only statements for which filter(stmt) is true are compiled. If
// lastPeriod is true, the final statement ends with a period.
func (c *Compiler) compileBlock(stmts []*parser.Statement, lastPeriod bool, filter func(*parser.Statement) bool) error {
	filtered := []*parser.Statement{}
	for _, s := range stmts {
		if filter == nil || filter(s) {
			filtered = append(filtered, s)
		}
	}
	for i, s := range filtered {
		c.writeIndent()
		if err := c.compileStmt(s); err != nil {
			return err
		}
		if i == len(filtered)-1 {
			if lastPeriod {
				c.buf.WriteString(".\n")
			} else {
				c.buf.WriteByte('\n')
			}
		} else {
			c.buf.WriteString(",\n")
		}
	}
	if len(filtered) == 0 && lastPeriod {
		c.writeln("ok.")
	}
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val := "undefined"
		if s.Let.Value != nil {
			v, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			val = v
		}
		c.buf.WriteString(fmt.Sprintf("%s = %s", s.Let.Name, val))
	case s.Var != nil:
		val := "undefined"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			val = v
		}
		c.buf.WriteString(fmt.Sprintf("%s = %s", s.Var.Name, val))
	case s.Assign != nil:
		if err := c.compileAssign(s.Assign); err != nil {
			return err
		}
	case s.Return != nil:
		v, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.buf.WriteString(fmt.Sprintf("throw({return, %s})", v))
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.buf.WriteString(expr)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.buf.WriteString("throw(mochi_break)")
		return nil
	case s.Continue != nil:
		c.buf.WriteString("throw(mochi_continue)")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	default:
		c.buf.WriteString("ok")
	}
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.buf.WriteString("if " + cond + " ->\n")
	c.indent++
	if err := c.compileBlock(stmt.Then, false, nil); err != nil {
		return err
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("; true ->\n")
		c.indent++
		if err := c.compileIf(stmt.ElseIf); err != nil {
			return err
		}
		c.indent--
	} else if len(stmt.Else) > 0 {
		c.writeIndent()
		c.buf.WriteString("; true ->\n")
		c.indent++
		if err := c.compileBlock(stmt.Else, false, nil); err != nil {
			return err
		}
		c.indent--
	}
	c.writeIndent()
	c.buf.WriteString("end")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	var list string
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		list = fmt.Sprintf("lists:seq(%s, (%s)-1)", start, end)
	} else {
		src, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		list = src
	}
	c.buf.WriteString(fmt.Sprintf("mochi_foreach(fun(%s) ->\n", stmt.Name))
	c.indent++
	if err := c.compileBlock(stmt.Body, false, nil); err != nil {
		return err
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("end, %s)", list))
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.buf.WriteString("mochi_while(fun() -> " + cond + " end, fun() ->\n")
	c.indent++
	if err := c.compileBlock(stmt.Body, false, nil); err != nil {
		return err
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("end)")
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.buf.WriteString(fmt.Sprintf("%s = %s", a.Name, val))
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	leftAst := b.Left
	left, err := c.compileUnary(leftAst)
	if err != nil {
		return "", err
	}
	out := left
	for _, op := range b.Right {
		rightAst := op.Right
		right, err := c.compilePostfix(rightAst)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			erlOp := op.Op
			if erlOp == "&&" {
				erlOp = "and"
			}
			if erlOp == "||" {
				erlOp = "or"
			}
			if erlOp == "==" {
				erlOp = "=="
			}
			if erlOp == "!=" {
				erlOp = "/="
			}
			if erlOp == "+" && (isListUnary(leftAst) || isListPostfix(rightAst)) {
				out = fmt.Sprintf("lists:append(%s, %s)", out, right)
			} else {
				out = fmt.Sprintf("(%s %s %s)", out, erlOp, right)
			}
		case "in":
			out = fmt.Sprintf("maps:is_key(%s, %s)", out, right)
		default:
			return "", fmt.Errorf("unsupported operator %s", op.Op)
		}
		leftAst = &parser.Unary{Value: rightAst}
	}
	return out, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			expr = "-" + expr
		case "!":
			expr = "not " + expr
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	res, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if isStringExpr(op.Index.Start) {
				c.needGet = true
				res = fmt.Sprintf("mochi_get(%s, %s)", res, idx)
			} else {
				res = fmt.Sprintf("lists:nth(%s + 1, %s)", idx, res)
			}
		} else if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, v)
			}
			argStr := strings.Join(args, ", ")
			switch res {
			case "print":
				res = fmt.Sprintf("mochi_print([%s])", argStr)
			case "len":
				res = fmt.Sprintf("length(%s)", argStr)
			case "str":
				res = fmt.Sprintf("mochi_format(%s)", argStr)
			case "count":
				res = fmt.Sprintf("mochi_count(%s)", argStr)
			case "avg":
				res = fmt.Sprintf("mochi_avg(%s)", argStr)
			case "input":
				res = "mochi_input()"
			default:
				res = fmt.Sprintf("%s(%s)", res, argStr)
			}
		} else if op.Cast != nil {
			// ignore type casts
		}
	}
	return res, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return fmt.Sprintf("%d", *p.Lit.Int), nil
		}
		if p.Lit.Str != nil {
			return fmt.Sprintf("\"%s\"", strings.ReplaceAll(*p.Lit.Str, "\"", "\\\"")), nil
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
		return "[" + strings.Join(elems, ", ") + "]", nil
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
			items[i] = fmt.Sprintf("%s => %s", k, v)
		}
		return "#{" + strings.Join(items, ", ") + "}", nil
	case p.Struct != nil:
		fields := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf("%s => %s", f.Name, v)
		}
		return "#{" + strings.Join(fields, ", ") + "}", nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.Selector != nil:
		name := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			for _, t := range p.Selector.Tail {
				name = fmt.Sprintf("maps:get(%s, %s)", t, name)
			}
			return name, nil
		}
		return name, nil
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, v)
		}
		argStr := strings.Join(args, ", ")
		switch p.Call.Func {
		case "print":
			return fmt.Sprintf("mochi_print([%s])", argStr), nil
		case "len":
			return fmt.Sprintf("length(%s)", argStr), nil
		case "str":
			return fmt.Sprintf("mochi_format(%s)", argStr), nil
		case "count":
			return fmt.Sprintf("mochi_count(%s)", argStr), nil
		case "avg":
			return fmt.Sprintf("mochi_avg(%s)", argStr), nil
		case "input":
			return "mochi_input()", nil
		default:
			return fmt.Sprintf("%s(%s)", p.Call.Func, argStr), nil
		}
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) > 0 || q.Group != nil {
		return "", fmt.Errorf("unsupported query expression")
	}
	if len(q.Froms) > 0 && (q.Sort != nil || q.Skip != nil || q.Take != nil) {
		return "", fmt.Errorf("unsupported query expression")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	orig := c.env
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, types.AnyType{}, true)
	for _, f := range q.Froms {
		child.SetVar(f.Var, types.AnyType{}, true)
	}
	c.env = child
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	c.env = orig

	if sortExpr == "" && skipExpr == "" && takeExpr == "" {
		var b strings.Builder
		b.WriteString("[")
		b.WriteString(sel)
		b.WriteString(" || ")
		b.WriteString(q.Var)
		b.WriteString(" <- ")
		b.WriteString(src)
		for _, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				return "", err
			}
			b.WriteString(", ")
			b.WriteString(f.Var)
			b.WriteString(" <- ")
			b.WriteString(fs)
		}
		if cond != "" {
			b.WriteString(", ")
			b.WriteString(cond)
		}
		b.WriteString("]")
		return b.String(), nil
	}

	var b strings.Builder
	b.WriteString("(fun() ->\n")
	b.WriteString("\tItems = [" + q.Var + " || " + q.Var + " <- " + src)
	for _, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		b.WriteString(", " + f.Var + " <- " + fs)
	}
	if cond != "" {
		b.WriteString(", " + cond)
	}
	b.WriteString("],\n")
	b.WriteString("\tSorted = ")
	if sortExpr != "" {
		b.WriteString("begin\n")
		b.WriteString("\t\tPairs = [{" + sortExpr + ", " + q.Var + "} || " + q.Var + " <- Items],\n")
		b.WriteString("\t\tSPairs = lists:sort(fun({A,_},{B,_}) -> A =< B end, Pairs),\n")
		b.WriteString("\t\t[ V || {_, V} <- SPairs ]\n")
		b.WriteString("\tend")
	} else {
		b.WriteString("Items")
	}
	b.WriteString(",\n")
	b.WriteString("\tSkipped = ")
	if skipExpr != "" {
		b.WriteString("(case " + skipExpr + " of\n")
		b.WriteString("\t\tN when N > 0 -> lists:nthtail(N, Sorted);\n")
		b.WriteString("\t\t_ -> Sorted\n")
		b.WriteString("\tend)")
	} else {
		b.WriteString("Sorted")
	}
	b.WriteString(",\n")
	b.WriteString("\tTaken = ")
	if takeExpr != "" {
		b.WriteString("lists:sublist(Skipped, " + takeExpr + ")")
	} else {
		b.WriteString("Skipped")
	}
	b.WriteString(",\n")
	b.WriteString("\t[" + sel + " || " + q.Var + " <- Taken]\n")
	b.WriteString("end)()")
	return b.String(), nil
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
	if p.Target != nil && p.Target.List != nil {
		return true
	}
	return false
}

func isStringExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Str == nil {
		return false
	}
	return true
}

func (c *Compiler) emitRuntime() {
	c.writeln("mochi_print(Args) ->")
	c.indent++
	c.writeln("Strs = [ mochi_format(A) || A <- Args ],")
	c.writeln("io:format(\"~s~n\", [lists:flatten(Strs)]).")
	c.indent--
	c.writeln("")

	c.writeln("mochi_format(X) when is_integer(X) -> integer_to_list(X);")
	c.writeln("mochi_format(X) when is_float(X) -> float_to_list(X);")
	c.writeln("mochi_format(X) when is_list(X) -> X;")
	c.writeln("mochi_format(X) -> lists:flatten(io_lib:format(\"~p\", [X])).")

	c.writeln("")
	c.writeln("mochi_count(X) when is_list(X) -> length(X);")
	c.writeln("mochi_count(X) when is_map(X) -> maps:size(X);")
	c.writeln("mochi_count(X) when is_binary(X) -> byte_size(X);")
	c.writeln("mochi_count(_) -> erlang:error(badarg).")

	c.writeln("")
	c.writeln("mochi_input() ->")
	c.indent++
	c.writeln("case io:get_line(\"\") of")
	c.indent++
	c.writeln("eof -> \"\";")
	c.writeln("Line -> string:trim(Line)")
	c.indent--
	c.writeln("end.")
	c.indent--

	c.writeln("")
	c.writeln("mochi_avg([]) -> 0;")
	c.writeln("mochi_avg(L) when is_list(L) ->")
	c.indent++
	c.writeln("Sum = lists:foldl(fun(X, Acc) ->")
	c.indent++
	c.writeln("case X of")
	c.indent++
	c.writeln("I when is_integer(I) -> Acc + I;")
	c.writeln("F when is_float(F) -> Acc + F;")
	c.writeln("_ -> erlang:error(badarg) end")
	c.indent--
	c.writeln("end, 0, L),")
	c.writeln("Sum / length(L)")
	c.indent--
	c.writeln(".")
	c.indent--
	c.writeln("mochi_avg(_) -> erlang:error(badarg).")

	c.writeln("")
	c.writeln("mochi_foreach(F, L) ->")
	c.indent++
	c.writeln("try mochi_foreach_loop(F, L) catch throw:mochi_break -> ok end.")
	c.indent--

	c.writeln("")
	c.writeln("mochi_foreach_loop(_, []) -> ok;")
	c.writeln("mochi_foreach_loop(F, [H|T]) ->")
	c.indent++
	c.writeln("try F(H) catch")
	c.indent++
	c.writeln("throw:mochi_continue -> ok;")
	c.writeln("throw:mochi_break -> throw(mochi_break)")
	c.indent--
	c.writeln("end,")
	c.writeln("mochi_foreach_loop(F, T).")
	c.indent--

	if c.needGet {
		c.writeln("")
		c.writeln("mochi_get(M, K) when is_list(M), is_integer(K) -> lists:nth(K + 1, M);")
		c.writeln("mochi_get(M, K) when is_map(M) -> maps:get(K, M);")
		c.writeln("mochi_get(_, _) -> erlang:error(badarg).")
	}

	c.writeln("")
	c.writeln("mochi_while(Cond, Body) ->")
	c.indent++
	c.writeln("case Cond() of")
	c.indent++
	c.writeln("true ->")
	c.indent++
	c.writeln("try Body() catch")
	c.indent++
	c.writeln("throw:mochi_continue -> ok;")
	c.writeln("throw:mochi_break -> ok")
	c.indent--
	c.writeln("end,")
	c.writeln("mochi_while(Cond, Body);")
	c.indent--
	c.writeln("_ -> ok")
	c.indent--
	c.writeln("end.")
	c.indent--
}
