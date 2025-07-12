//go:build archived

package plcode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

type exprRes struct {
	code []string
	val  string
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

func (c *Compiler) compileExpr(e *parser.Expr) (exprRes, error) {
	if e == nil {
		return exprRes{val: ""}, nil
	}
	return c.compileBinary(e.Binary)
}

type operand struct {
	expr   exprRes
	isList bool
	isStr  bool
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
		if left.isStr && right.isStr {
			tmp := c.newVar()
			res.code = append(res.code, fmt.Sprintf("string_concat(%s, %s, %s)", left.expr.val, right.expr.val, tmp)+",")
			res.val = tmp
			return operand{expr: res, isStr: true}, nil
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
		res.code = append(res.code, fmt.Sprintf("%s is %s / %s,", tmp, left.expr.val, right.expr.val))
		res.val = tmp
		return operand{expr: res}, nil
	case "%":
		tmp := c.newVar()
		res.code = append(res.code, fmt.Sprintf("%s is %s mod %s,", tmp, left.expr.val, right.expr.val))
		res.val = tmp
		return operand{expr: res}, nil
	case "union_all":
		tmp := c.newVar()
		res.code = append(res.code, fmt.Sprintf("append(%s, %s, %s)", left.expr.val, right.expr.val, tmp)+",")
		res.val = tmp
		return operand{expr: res, isList: true}, nil
	case "union":
		tmp := c.newVar()
		c.use("union")
		res.code = append(res.code, fmt.Sprintf("union(%s, %s, %s),", left.expr.val, right.expr.val, tmp))
		res.val = tmp
		return operand{expr: res, isList: true}, nil
	case "except":
		tmp := c.newVar()
		c.use("except")
		res.code = append(res.code, fmt.Sprintf("except(%s, %s, %s),", left.expr.val, right.expr.val, tmp))
		res.val = tmp
		return operand{expr: res, isList: true}, nil
	case "intersect":
		tmp := c.newVar()
		c.use("intersect")
		res.code = append(res.code, fmt.Sprintf("intersect(%s, %s, %s),", left.expr.val, right.expr.val, tmp))
		res.val = tmp
		return operand{expr: res, isList: true}, nil
	case "==", "!=", "<", "<=", ">", ">=":
		var opStr string
		switch op {
		case "==":
			if left.isStr && right.isStr {
				opStr = fmt.Sprintf("%s == %s", left.expr.val, right.expr.val)
			} else {
				opStr = fmt.Sprintf("%s = %s", left.expr.val, right.expr.val)
			}
		case "!=":
			if left.isStr && right.isStr {
				opStr = fmt.Sprintf("%s \\== %s", left.expr.val, right.expr.val)
			} else {
				opStr = fmt.Sprintf("%s \\= %s", left.expr.val, right.expr.val)
			}
		case "<":
			opStr = fmt.Sprintf("%s @< %s", left.expr.val, right.expr.val)
		case "<=":
			opStr = fmt.Sprintf("%s @=< %s", left.expr.val, right.expr.val)
		case ">":
			opStr = fmt.Sprintf("%s @> %s", left.expr.val, right.expr.val)
		case ">=":
			opStr = fmt.Sprintf("%s @>= %s", left.expr.val, right.expr.val)
		}
		tmp := c.newVar()
		res.code = append(res.code, fmt.Sprintf("(%s -> %s = true ; %s = false),", opStr, tmp, tmp))
		res.val = tmp
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
	operands := []operand{{expr: first, isList: isListExpr(b.Left), isStr: c.isStringUnary(b.Left)}}
	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return exprRes{}, err
		}
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		operands = append(operands, operand{expr: r, isList: isListPostfix(part.Right), isStr: c.isStringPostfix(part.Right)})
		ops = append(ops, op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
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
	var res exprRes
	target := p.Target
	ops := p.Ops
	// Detect method call: selector followed by call
	if len(ops) > 0 && ops[0].Call != nil && target.Selector != nil && len(target.Selector.Tail) > 0 {
		method := target.Selector.Tail[len(target.Selector.Tail)-1]
		rootSel := *target.Selector
		rootSel.Tail = rootSel.Tail[:len(rootSel.Tail)-1]
		rootExpr := &parser.Primary{Selector: &rootSel}
		rootRes, err := c.compilePrimary(rootExpr)
		if err != nil {
			return exprRes{}, err
		}
		args := make([]string, len(ops[0].Call.Args)+1)
		code := append([]string{}, rootRes.code...)
		args[0] = rootRes.val
		for i, a := range ops[0].Call.Args {
			ar, err := c.compileExpr(a)
			if err != nil {
				return exprRes{}, err
			}
			code = append(code, ar.code...)
			args[i+1] = ar.val
		}
		tmp := c.newVar()
		if method == "contains" {
			c.use("contains")
		}
		if method == "starts_with" {
			c.use("starts_with")
		}
		code = append(code, fmt.Sprintf("%s(%s, %s),", sanitizeAtom(method), strings.Join(args, ", "), tmp))
		res = exprRes{code: code, val: tmp}
		ops = ops[1:]
	} else {
		var err error
		res, err = c.compilePrimary(target)
		if err != nil {
			return exprRes{}, err
		}
	}
	for _, op := range ops {
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

func (c *Compiler) compileSelector(sel *parser.SelectorExpr) (exprRes, error) {
	var res exprRes
	var cur string
	if name, ok := c.vars[sel.Root]; ok {
		cur = c.newVar()
		res.code = append(res.code, fmt.Sprintf("nb_getval(%s, %s)", name, cur)+",")
	} else if c.env != nil {
		if _, ok := c.env.GetFunc(sel.Root); ok {
			cur = sanitizeAtom(sel.Root)
		} else {
			cur = sanitizeVar(sel.Root)
		}
	} else {
		cur = sanitizeVar(sel.Root)
	}
	if len(sel.Tail) == 0 && c.env != nil {
		if t, err := c.env.GetVar(sel.Root); err == nil {
			if _, ok := t.(types.GroupType); ok {
				tmp := c.newVar()
				res.code = append(res.code, fmt.Sprintf("get_dict('Items', %s, %s)", cur, tmp)+",")
				cur = tmp
			}
		}
	}
	for _, field := range sel.Tail {
		tmp := c.newVar()
		res.code = append(res.code, fmt.Sprintf("get_dict(%s, %s, %s)", sanitizeAtom(field), cur, tmp)+",")
		cur = tmp
	}
	res.val = cur
	return res, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (exprRes, error) {
	switch {
	case p.Selector != nil:
		return c.compileSelector(p.Selector)
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
		pairs := []string{}
		code := []string{}
		for _, it := range p.Map.Items {
			kr, err := c.compileExpr(it.Key)
			if err != nil {
				return exprRes{}, err
			}
			vr, err := c.compileExpr(it.Value)
			if err != nil {
				return exprRes{}, err
			}
			code = append(code, kr.code...)
			code = append(code, vr.code...)
			keyVal := kr.val
			if id, ok := identName(it.Key); ok {
				keyVal = sanitizeAtom(id)
			} else if isStringLiteral(it.Key) {
				keyVal = "'" + strings.Trim(kr.val, "\"") + "'"
			}
			pairs = append(pairs, fmt.Sprintf("%s-%s", keyVal, vr.val))
		}
		tmp := c.newVar()
		code = append(code, fmt.Sprintf("dict_create(%s, map, [%s]),", tmp, strings.Join(pairs, ", ")))
		return exprRes{code: code, val: tmp}, nil
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.LogicQuery != nil:
		return c.compileLogicQuery(p.LogicQuery)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
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
	case "count":
		if len(call.Args) != 1 {
			return exprRes{}, fmt.Errorf("count expects 1 arg")
		}
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		tmp := c.newVar()
		c.use("count")
		code := append(arg.code, fmt.Sprintf("count(%s, %s),", arg.val, tmp))
		return exprRes{code: code, val: tmp}, nil
	case "sum":
		if len(call.Args) != 1 {
			return exprRes{}, fmt.Errorf("sum expects 1 arg")
		}
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		tmp := c.newVar()
		c.use("sum")
		code := append(arg.code, fmt.Sprintf("sum(%s, %s),", arg.val, tmp))
		return exprRes{code: code, val: tmp}, nil
	case "avg":
		if len(call.Args) != 1 {
			return exprRes{}, fmt.Errorf("avg expects 1 arg")
		}
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		tmp := c.newVar()
		c.use("avg")
		code := append(arg.code, fmt.Sprintf("avg(%s, %s),", arg.val, tmp))
		return exprRes{code: code, val: tmp}, nil
	case "min":
		if len(call.Args) != 1 {
			return exprRes{}, fmt.Errorf("min expects 1 arg")
		}
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		tmp := c.newVar()
		c.use("min")
		code := append(arg.code, fmt.Sprintf("min(%s, %s),", arg.val, tmp))
		return exprRes{code: code, val: tmp}, nil
	case "keys":
		if len(call.Args) != 1 {
			return exprRes{}, fmt.Errorf("keys expects 1 arg")
		}
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		tmp := c.newVar()
		c.use("map_keys")
		code := append(arg.code, fmt.Sprintf("map_keys(%s, %s),", arg.val, tmp))
		return exprRes{code: code, val: tmp}, nil
	case "str":
		if len(call.Args) != 1 {
			return exprRes{}, fmt.Errorf("str expects 1 arg")
		}
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		tmp := c.newVar()
		code := append(arg.code, fmt.Sprintf("term_string(%s, %s),", arg.val, tmp))
		return exprRes{code: code, val: tmp}, nil
	case "input":
		if len(call.Args) != 0 {
			return exprRes{}, fmt.Errorf("input expects no args")
		}
		tmp := c.newVar()
		c.use("input")
		code := []string{fmt.Sprintf("input(%s),", tmp)}
		return exprRes{code: code, val: tmp}, nil
	case "json":
		if len(call.Args) != 1 {
			return exprRes{}, fmt.Errorf("json expects 1 arg")
		}
		arg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		c.use("json")
		code := append(arg.code, fmt.Sprintf("json(%s),", arg.val))
		return exprRes{code: code, val: ""}, nil
	case "dataset_filter":
		if len(call.Args) != 2 {
			return exprRes{}, fmt.Errorf("dataset_filter expects 2 args")
		}
		listArg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		predArg, err := c.compileExpr(call.Args[1])
		if err != nil {
			return exprRes{}, err
		}
		tmp := c.newVar()
		c.use("dataset_filter")
		code := append(listArg.code, predArg.code...)
		code = append(code, fmt.Sprintf("dataset_filter(%s, %s, %s),", listArg.val, predArg.val, tmp))
		return exprRes{code: code, val: tmp}, nil
	case "dataset_paginate":
		if len(call.Args) != 3 {
			return exprRes{}, fmt.Errorf("dataset_paginate expects 3 args")
		}
		listArg, err := c.compileExpr(call.Args[0])
		if err != nil {
			return exprRes{}, err
		}
		skipArg, err := c.compileExpr(call.Args[1])
		if err != nil {
			return exprRes{}, err
		}
		takeArg, err := c.compileExpr(call.Args[2])
		if err != nil {
			return exprRes{}, err
		}
		tmp := c.newVar()
		c.use("dataset_paginate")
		code := append(listArg.code, skipArg.code...)
		code = append(code, takeArg.code...)
		code = append(code, fmt.Sprintf("dataset_paginate(%s, %s, %s, %s),", listArg.val, skipArg.val, takeArg.val, tmp))
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
		if fn, ok := c.funVars[call.Func]; ok {
			argStr := strings.Join(args, ", ")
			var callLine string
			if len(args) == 0 {
				callLine = fmt.Sprintf("%s(%s)", fn, tmp)
			} else {
				callLine = fmt.Sprintf("%s(%s, %s)", fn, argStr, tmp)
			}
			code = append(code, callLine+",")
			return exprRes{code: code, val: tmp}, nil
		}
		if c.env != nil {
			if _, ok := c.env.GetFunc(call.Func); ok {
				argStr := strings.Join(args, ", ")
				var callLine string
				if len(args) == 0 {
					callLine = fmt.Sprintf("%s(%s)", sanitizeAtom(call.Func), tmp)
				} else {
					callLine = fmt.Sprintf("%s(%s, %s)", sanitizeAtom(call.Func), argStr, tmp)
				}
				code = append(code, callLine+",")
				return exprRes{code: code, val: tmp}, nil
			}
		}
		allArgs := append([]string{sanitizeVar(call.Func)}, args...)
		allArgs = append(allArgs, tmp)
		callLine := fmt.Sprintf("call(%s)", strings.Join(allArgs, ", "))
		code = append(code, callLine+",")
		return exprRes{code: code, val: tmp}, nil
	}
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (exprRes, error) {
	if q.Group != nil {
		if len(q.Froms) == 0 {
			if len(q.Joins) == 0 {
				return c.compileGroupedQueryExpr(q)
			}
			return c.compileGroupedQueryWithJoins(q)
		}
		return exprRes{}, fmt.Errorf("unsupported query expression")
	}
	return c.compileSimpleQueryExpr(q)
}

func (c *Compiler) compileSimpleQueryExpr(q *parser.QueryExpr) (exprRes, error) {
	for _, j := range q.Joins {
		if j.Side != nil {
			return exprRes{}, fmt.Errorf("unsupported join side")
		}
	}

	src, err := c.compileExpr(q.Source)
	if err != nil {
		return exprRes{}, err
	}

	child := types.NewEnv(c.env)
	var elem types.Type = types.AnyType{}
	if lt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.ListType); ok {
		elem = lt.Elem
	}
	child.SetVar(q.Var, elem, true)
	for _, f := range q.Froms {
		var ft types.Type = types.AnyType{}
		if lt, ok := types.TypeOfExprBasic(f.Src, c.env).(types.ListType); ok {
			ft = lt.Elem
		}
		child.SetVar(f.Var, ft, true)
	}
	for _, j := range q.Joins {
		var jt types.Type = types.AnyType{}
		if lt, ok := types.TypeOfExprBasic(j.Src, c.env).(types.ListType); ok {
			jt = lt.Elem
		}
		child.SetVar(j.Var, jt, true)
	}
	orig := c.env
	c.env = child

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return exprRes{}, err
	}
	var cond exprRes
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return exprRes{}, err
		}
	}
	var sortRes exprRes
	if q.Sort != nil {
		sortRes, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return exprRes{}, err
		}
	}
	var skipRes, takeRes exprRes
	if q.Skip != nil {
		skipRes, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return exprRes{}, err
		}
	}
	if q.Take != nil {
		takeRes, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return exprRes{}, err
		}
	}
	c.env = orig

	listVar := c.newVar()
	code := append([]string{}, src.code...)
	c.use("tolist")
	code = append(code, fmt.Sprintf("to_list(%s, %s),", src.val, listVar))

	filtered := false
	if q.Where != nil {
		varsUsed := exprVars(q.Where)
		if len(varsUsed) == 1 {
			if _, ok := varsUsed[q.Var]; ok {
				fenv := types.NewEnv(nil)
				fenv.SetVar(q.Var, elem, true)
				old := c.env
				c.env = fenv
				fcond, err := c.compileExpr(q.Where)
				c.env = old
				if err != nil {
					return exprRes{}, err
				}
				item := sanitizeVar(q.Var)
				parts := []string{fmt.Sprintf("member(%s, %s)", item, listVar)}
				for _, line := range fcond.code {
					parts = append(parts, strings.TrimSuffix(line, ","))
				}
				parts = append(parts, fcond.val)
				goal := strings.Join(parts, ", ")
				outVar := c.newVar()
				code = append(code, fmt.Sprintf("findall(%s, (%s), %s),", item, goal, outVar))
				listVar = outVar
				filtered = true
			}
		}
	}

	fromLists := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fr, err := c.compileExpr(f.Src)
		if err != nil {
			return exprRes{}, err
		}
		code = append(code, fr.code...)
		lv := c.newVar()
		c.use("tolist")
		code = append(code, fmt.Sprintf("to_list(%s, %s),", fr.val, lv))
		fromLists[i] = lv
	}

	joinLists := make([]string, len(q.Joins))
	joinConds := make([][]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return exprRes{}, err
		}
		onRes, err := c.compileExpr(j.On)
		if err != nil {
			return exprRes{}, err
		}
		code = append(code, js.code...)
		lv := c.newVar()
		c.use("tolist")
		code = append(code, fmt.Sprintf("to_list(%s, %s),", js.val, lv))
		joinLists[i] = lv
		tmp := []string{}
		for _, line := range onRes.code {
			tmp = append(tmp, strings.TrimSuffix(line, ","))
		}
		tmp = append(tmp, onRes.val)
		joinConds[i] = tmp
	}

	resultVar := c.newVar()
	innerParts := []string{fmt.Sprintf("member(%s, %s)", sanitizeVar(q.Var), listVar)}
	for i, lv := range fromLists {
		innerParts = append(innerParts, fmt.Sprintf("member(%s, %s)", sanitizeVar(q.Froms[i].Var), lv))
	}
	for i, lv := range joinLists {
		innerParts = append(innerParts, fmt.Sprintf("member(%s, %s)", sanitizeVar(q.Joins[i].Var), lv))
		for _, l := range joinConds[i] {
			innerParts = append(innerParts, l)
		}
	}

	if !filtered {
		for _, line := range cond.code {
			innerParts = append(innerParts, strings.TrimSuffix(line, ","))
		}
		if q.Where != nil {
			innerParts = append(innerParts, cond.val)
		}
	}
	for _, line := range sel.code {
		innerParts = append(innerParts, strings.TrimSuffix(line, ","))
	}

	if q.Sort == nil {
		innerParts = append(innerParts, fmt.Sprintf("%s = %s", resultVar, sel.val))
		goal := strings.Join(innerParts, ", ")
		itemsVar := c.newVar()
		code = append(code, fmt.Sprintf("findall(%s, (%s), %s),", resultVar, goal, itemsVar))
		resultVar = itemsVar
	} else {
		keyVar := c.newVar()
		for _, line := range sortRes.code {
			innerParts = append(innerParts, strings.TrimSuffix(line, ","))
		}
		innerParts = append(innerParts, fmt.Sprintf("%s = %s", keyVar, sortRes.val))
		innerParts = append(innerParts, fmt.Sprintf("%s = %s", resultVar, sel.val))
		goal := strings.Join(innerParts, ", ")
		pairsVar := c.newVar()
		sortedVar := c.newVar()
		itemsVar := c.newVar()
		code = append(code, fmt.Sprintf("findall(%s-%s, (%s), %s),", keyVar, resultVar, goal, pairsVar))
		code = append(code, fmt.Sprintf("keysort(%s, %s),", pairsVar, sortedVar))
		code = append(code, fmt.Sprintf("findall(V, member(_-V, %s), %s),", sortedVar, itemsVar))
		resultVar = itemsVar
	}

	if q.Skip != nil || q.Take != nil {
		for _, line := range skipRes.code {
			code = append(code, line)
		}
		for _, line := range takeRes.code {
			code = append(code, line)
		}
		lenVar := c.newVar()
		code = append(code, fmt.Sprintf("length(%s, %s),", resultVar, lenVar))
		startVar := "0"
		if q.Skip != nil {
			startVar = skipRes.val
		}
		endVar := lenVar
		if q.Take != nil {
			endVar = c.newVar()
			code = append(code, fmt.Sprintf("%s is %s + %s,", endVar, startVar, takeRes.val))
		}
		finalVar := c.newVar()
		c.use("slice")
		code = append(code, fmt.Sprintf("slice(%s, %s, %s, %s),", resultVar, startVar, endVar, finalVar))
		resultVar = finalVar
	}

	return exprRes{code: code, val: resultVar}, nil
}

func (c *Compiler) compileGroupedQueryWithJoins(q *parser.QueryExpr) (exprRes, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return exprRes{}, err
	}

	child := types.NewEnv(c.env)
	var elem types.Type = types.AnyType{}
	if lt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.ListType); ok {
		elem = lt.Elem
	}
	child.SetVar(q.Var, elem, true)
	for _, f := range q.Froms {
		child.SetVar(f.Var, types.AnyType{}, true)
	}
	for _, j := range q.Joins {
		child.SetVar(j.Var, types.AnyType{}, true)
	}
	origEnv := c.env
	c.env = child
	var condRes exprRes
	if q.Where != nil {
		condRes, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = origEnv
			return exprRes{}, err
		}
	}
	keyRes, err := c.compileExpr(q.Group.Exprs[0])
	if err != nil {
		c.env = origEnv
		return exprRes{}, err
	}
	c.env = origEnv

	// source list
	listVar := c.newVar()
	code := append([]string{}, src.code...)
	c.use("tolist")
	code = append(code, fmt.Sprintf("to_list(%s, %s),", src.val, listVar))

	// from clauses
	fromLists := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fr, err := c.compileExpr(f.Src)
		if err != nil {
			return exprRes{}, err
		}
		code = append(code, fr.code...)
		lv := c.newVar()
		c.use("tolist")
		code = append(code, fmt.Sprintf("to_list(%s, %s),", fr.val, lv))
		fromLists[i] = lv
	}

	// join clauses
	joinLists := make([]string, len(q.Joins))
	joinConds := make([][]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return exprRes{}, err
		}
		onRes, err := c.compileExpr(j.On)
		if err != nil {
			return exprRes{}, err
		}
		code = append(code, js.code...)
		lv := c.newVar()
		c.use("tolist")
		code = append(code, fmt.Sprintf("to_list(%s, %s),", js.val, lv))
		joinLists[i] = lv
		tmp := []string{}
		for _, line := range onRes.code {
			tmp = append(tmp, strings.TrimSuffix(line, ","))
		}
		tmp = append(tmp, onRes.val)
		joinConds[i] = tmp
	}

	// build key-item pairs
	keyVar := c.newVar()
	pairVar := c.newVar()
	innerParts := []string{fmt.Sprintf("member(%s, %s)", sanitizeVar(q.Var), listVar)}
	for i, lv := range fromLists {
		innerParts = append(innerParts, fmt.Sprintf("member(%s, %s)", sanitizeVar(q.Froms[i].Var), lv))
	}
	for i, lv := range joinLists {
		innerParts = append(innerParts, fmt.Sprintf("member(%s, %s)", sanitizeVar(q.Joins[i].Var), lv))
		for _, l := range joinConds[i] {
			innerParts = append(innerParts, l)
		}
	}
	if q.Where != nil {
		for _, line := range condRes.code {
			innerParts = append(innerParts, strings.TrimSuffix(line, ","))
		}
		innerParts = append(innerParts, condRes.val)
	}
	for _, line := range keyRes.code {
		innerParts = append(innerParts, strings.TrimSuffix(line, ","))
	}
	innerParts = append(innerParts, fmt.Sprintf("%s = %s", keyVar, keyRes.val))
	innerParts = append(innerParts, fmt.Sprintf("%s = %s-%s", pairVar, keyVar, sanitizeVar(q.Var)))
	goal := strings.Join(innerParts, ", ")
	pairsVar := c.newVar()
	c.use("group_by")
	code = append(code, fmt.Sprintf("findall(%s, (%s), %s),", pairVar, goal, pairsVar))

	groupsVar := c.newVar()
	code = append(code, fmt.Sprintf("group_pairs(%s, [], %s),", pairsVar, groupsVar))

	genv := types.NewEnv(c.env)
	genv.SetVar(q.Group.Name, types.GroupType{Elem: elem}, true)
	c.env = genv
	selRes, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = origEnv
		return exprRes{}, err
	}
	var sortRes, skipRes, takeRes exprRes
	if q.Sort != nil {
		sortRes, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = origEnv
			return exprRes{}, err
		}
	}
	if q.Skip != nil {
		skipRes, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = origEnv
			return exprRes{}, err
		}
	}
	if q.Take != nil {
		takeRes, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = origEnv
			return exprRes{}, err
		}
	}
	c.env = origEnv

	resultVar := c.newVar()
	parts := []string{fmt.Sprintf("member(%s, %s)", sanitizeVar(q.Group.Name), groupsVar)}
	for _, line := range selRes.code {
		parts = append(parts, strings.TrimSuffix(line, ","))
	}
	if q.Sort == nil {
		parts = append(parts, fmt.Sprintf("%s = %s", resultVar, selRes.val))
		goal = strings.Join(parts, ", ")
		outVar := c.newVar()
		code = append(code, fmt.Sprintf("findall(%s, (%s), %s),", resultVar, goal, outVar))
		resultVar = outVar
	} else {
		keyVar := c.newVar()
		for _, line := range sortRes.code {
			parts = append(parts, strings.TrimSuffix(line, ","))
		}
		parts = append(parts, fmt.Sprintf("%s = %s", keyVar, sortRes.val))
		parts = append(parts, fmt.Sprintf("%s = %s", resultVar, selRes.val))
		goal = strings.Join(parts, ", ")
		pairsVar := c.newVar()
		sortedVar := c.newVar()
		itemsVar := c.newVar()
		code = append(code, fmt.Sprintf("findall(%s-%s, (%s), %s),", keyVar, resultVar, goal, pairsVar))
		code = append(code, fmt.Sprintf("keysort(%s, %s),", pairsVar, sortedVar))
		code = append(code, fmt.Sprintf("findall(V, member(_-V, %s), %s),", sortedVar, itemsVar))
		resultVar = itemsVar
	}

	if q.Skip != nil || q.Take != nil {
		for _, line := range skipRes.code {
			code = append(code, line)
		}
		for _, line := range takeRes.code {
			code = append(code, line)
		}
		lenVar := c.newVar()
		code = append(code, fmt.Sprintf("length(%s, %s),", resultVar, lenVar))
		startVar := "0"
		if q.Skip != nil {
			startVar = skipRes.val
		}
		endVar := lenVar
		if q.Take != nil {
			endVar = c.newVar()
			code = append(code, fmt.Sprintf("%s is %s + %s,", endVar, startVar, takeRes.val))
		}
		finalVar := c.newVar()
		c.use("slice")
		code = append(code, fmt.Sprintf("slice(%s, %s, %s, %s),", resultVar, startVar, endVar, finalVar))
		resultVar = finalVar
	}

	return exprRes{code: code, val: resultVar}, nil
}

func (c *Compiler) compileGroupedQueryExpr(q *parser.QueryExpr) (exprRes, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return exprRes{}, err
	}

	child := types.NewEnv(c.env)
	var elem types.Type = types.AnyType{}
	if lt, ok := types.TypeOfExprBasic(q.Source, c.env).(types.ListType); ok {
		elem = lt.Elem
	}
	child.SetVar(q.Var, elem, true)
	origEnv := c.env
	c.env = child
	var condRes exprRes
	if q.Where != nil {
		lambda := &parser.FunExpr{Params: []*parser.Param{{Name: q.Var}}, ExprBody: q.Where}
		condRes, err = c.compileFunExpr(lambda)
		if err != nil {
			c.env = origEnv
			return exprRes{}, err
		}
	}
	keyLambdaExpr := &parser.FunExpr{Params: []*parser.Param{{Name: q.Var}}, ExprBody: q.Group.Exprs[0]}
	keyRes, err := c.compileFunExpr(keyLambdaExpr)
	if err != nil {
		c.env = origEnv
		return exprRes{}, err
	}
	c.env = origEnv

	genv := types.NewEnv(c.env)
	genv.SetVar(q.Group.Name, types.GroupType{Elem: elem}, true)
	c.env = genv
	selRes, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = origEnv
		return exprRes{}, err
	}
	var sortRes, skipRes, takeRes exprRes
	if q.Sort != nil {
		sortRes, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = origEnv
			return exprRes{}, err
		}
	}
	if q.Skip != nil {
		skipRes, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = origEnv
			return exprRes{}, err
		}
	}
	if q.Take != nil {
		takeRes, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = origEnv
			return exprRes{}, err
		}
	}
	c.env = origEnv

	listVar := c.newVar()
	code := append([]string{}, src.code...)
	c.use("tolist")
	code = append(code, fmt.Sprintf("to_list(%s, %s),", src.val, listVar))

	if q.Where != nil {
		filtered := c.newVar()
		c.use("dataset_filter")
		code = append(code, fmt.Sprintf("dataset_filter(%s, %s, %s),", listVar, condRes.val, filtered))
		listVar = filtered
	}

	groupsVar := c.newVar()
	c.use("group_by")
	code = append(code, fmt.Sprintf("group_by(%s, %s, %s),", listVar, keyRes.val, groupsVar))

	resultVar := c.newVar()
	parts := []string{fmt.Sprintf("member(%s, %s)", sanitizeVar(q.Group.Name), groupsVar)}
	for _, line := range selRes.code {
		parts = append(parts, strings.TrimSuffix(line, ","))
	}
	if q.Sort == nil {
		parts = append(parts, fmt.Sprintf("%s = %s", resultVar, selRes.val))
		goal := strings.Join(parts, ", ")
		outVar := c.newVar()
		code = append(code, fmt.Sprintf("findall(%s, (%s), %s),", resultVar, goal, outVar))
		resultVar = outVar
	} else {
		keyVar := c.newVar()
		for _, line := range sortRes.code {
			parts = append(parts, strings.TrimSuffix(line, ","))
		}
		parts = append(parts, fmt.Sprintf("%s = %s", keyVar, sortRes.val))
		parts = append(parts, fmt.Sprintf("%s = %s", resultVar, selRes.val))
		goal := strings.Join(parts, ", ")
		pairsVar := c.newVar()
		sortedVar := c.newVar()
		itemsVar := c.newVar()
		code = append(code, fmt.Sprintf("findall(%s-%s, (%s), %s),", keyVar, resultVar, goal, pairsVar))
		code = append(code, fmt.Sprintf("keysort(%s, %s),", pairsVar, sortedVar))
		code = append(code, fmt.Sprintf("findall(V, member(_-V, %s), %s),", sortedVar, itemsVar))
		resultVar = itemsVar
	}

	if q.Skip != nil || q.Take != nil {
		for _, line := range skipRes.code {
			code = append(code, line)
		}
		for _, line := range takeRes.code {
			code = append(code, line)
		}
		lenVar := c.newVar()
		code = append(code, fmt.Sprintf("length(%s, %s),", resultVar, lenVar))
		startVar := "0"
		if q.Skip != nil {
			startVar = skipRes.val
		}
		endVar := lenVar
		if q.Take != nil {
			endVar = c.newVar()
			code = append(code, fmt.Sprintf("%s is %s + %s,", endVar, startVar, takeRes.val))
		}
		finalVar := c.newVar()
		c.use("slice")
		code = append(code, fmt.Sprintf("slice(%s, %s, %s, %s),", resultVar, startVar, endVar, finalVar))
		resultVar = finalVar
	}

	return exprRes{code: code, val: resultVar}, nil
}

func (c *Compiler) compileLogicQuery(q *parser.LogicQueryExpr) (exprRes, error) {
	pred, fields, err := c.logicPredicate(q.Pred)
	if err != nil {
		return exprRes{}, err
	}
	tmp := c.newVar()
	dict := "_{" + strings.Join(fields, ", ") + "}"
	code := []string{fmt.Sprintf("findall(%s, %s, %s),", dict, pred, tmp)}
	return exprRes{code: code, val: tmp}, nil
}

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (exprRes, error) {
	pairs := make([]string, len(s.Fields))
	code := []string{}
	for i, f := range s.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return exprRes{}, err
		}
		code = append(code, v.code...)
		pairs[i] = fmt.Sprintf("%s-%s", sanitizeAtom(f.Name), v.val)
	}
	tmp := c.newVar()
	tag := sanitizeAtom(s.Name)
	code = append(code, fmt.Sprintf("dict_create(%s, %s, [%s]),", tmp, tag, strings.Join(pairs, ", ")))
	return exprRes{code: code, val: tmp}, nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (exprRes, error) {
	name := fmt.Sprintf("_lambda%d", len(c.lambdas))
	oldBuf := c.buf
	oldIndent := c.indent
	c.buf = bytes.Buffer{}
	c.indent = 0
	var body []*parser.Statement
	if fn.ExprBody != nil {
		body = []*parser.Statement{{Return: &parser.ReturnStmt{Value: fn.ExprBody}}}
	} else {
		body = fn.BlockBody
	}
	fs := &parser.FunStmt{Name: name, Params: fn.Params, Return: fn.Return, Body: body}
	if err := c.compileFun(fs); err != nil {
		c.buf = oldBuf
		c.indent = oldIndent
		return exprRes{}, err
	}
	code := c.buf.String()
	c.lambdas = append(c.lambdas, code)
	c.buf = oldBuf
	c.indent = oldIndent
	return exprRes{val: sanitizeAtom(name)}, nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (exprRes, error) {
	path := "\"\""
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	opts := "_{}"
	var code []string
	if l.With != nil {
		o, err := c.compileExpr(l.With)
		if err != nil {
			return exprRes{}, err
		}
		code = append(code, o.code...)
		opts = o.val
	}
	tmp := c.newVar()
	c.use("load_data")
	code = append(code, fmt.Sprintf("load_data(%s, %s, %s),", path, opts, tmp))
	return exprRes{code: code, val: tmp}, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (exprRes, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return exprRes{}, err
	}
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "_{}"
	code := append([]string{}, src.code...)
	if s.With != nil {
		o, err := c.compileExpr(s.With)
		if err != nil {
			return exprRes{}, err
		}
		code = append(code, o.code...)
		opts = o.val
	}
	c.use("save_data")
	code = append(code, fmt.Sprintf("save_data(%s, %s, %s),", src.val, path, opts))
	return exprRes{code: code, val: ""}, nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (exprRes, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return exprRes{}, err
	}
	opts := "_{}"
	code := append([]string{}, url.code...)
	if f.With != nil {
		o, err := c.compileExpr(f.With)
		if err != nil {
			return exprRes{}, err
		}
		code = append(code, o.code...)
		opts = o.val
	}
	tmp := c.newVar()
	c.use("fetch_data")
	code = append(code, fmt.Sprintf("fetch_data(%s, %s, %s),", url.val, opts, tmp))
	return exprRes{code: code, val: tmp}, nil
}

func (c *Compiler) logicPredicate(p *parser.LogicPredicate) (string, []string, error) {
	args := make([]string, len(p.Args))
	fields := []string{}
	for i, a := range p.Args {
		arg, field, err := c.logicTerm(a)
		if err != nil {
			return "", nil, err
		}
		args[i] = arg
		if field != "" {
			fields = append(fields, field)
		}
	}
	return fmt.Sprintf("%s(%s)", sanitizeAtom(p.Name), strings.Join(args, ", ")), fields, nil
}

func (c *Compiler) logicTerm(t *parser.LogicTerm) (string, string, error) {
	switch {
	case t.Var != nil:
		v := sanitizeVar(*t.Var)
		return v, fmt.Sprintf("%s:%s", *t.Var, v), nil
	case t.Str != nil:
		return fmt.Sprintf("%q", *t.Str), "", nil
	case t.Int != nil:
		return fmt.Sprintf("%d", *t.Int), "", nil
	default:
		return "", "", fmt.Errorf("invalid logic term")
	}
}

func (c *Compiler) compileFact(f *parser.FactStmt) error {
	pred, _, err := c.logicPredicate(f.Pred)
	if err != nil {
		return err
	}
	c.writeln(pred + ".")
	return nil
}

func (c *Compiler) compileRule(r *parser.RuleStmt) error {
	head, _, err := c.logicPredicate(r.Head)
	if err != nil {
		return err
	}
	c.writeln(head + " :-")
	c.indent++
	for i, cnd := range r.Body {
		var part string
		if cnd.Pred != nil {
			p, _, err := c.logicPredicate(cnd.Pred)
			if err != nil {
				return err
			}
			part = p
		} else if cnd.Neq != nil {
			part = fmt.Sprintf("%s \\= %s", sanitizeVar(cnd.Neq.A), sanitizeVar(cnd.Neq.B))
		} else {
			return fmt.Errorf("invalid rule condition")
		}
		if i == len(r.Body)-1 {
			c.writeln(part + ".")
		} else {
			c.writeln(part + ",")
		}
	}
	c.indent--
	return nil
}
