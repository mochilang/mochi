package plcode

import (
	"fmt"
	"strings"

	"mochi/parser"
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

func (c *Compiler) compileSelector(sel *parser.SelectorExpr) (exprRes, error) {
	var res exprRes
	var cur string
	if name, ok := c.vars[sel.Root]; ok {
		cur = c.newVar()
		res.code = append(res.code, fmt.Sprintf("nb_getval(%s, %s)", name, cur)+",")
	} else {
		cur = sanitizeVar(sel.Root)
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
			pairs = append(pairs, fmt.Sprintf("%s-%s", kr.val, vr.val))
		}
		tmp := c.newVar()
		code = append(code, fmt.Sprintf("dict_create(%s, _, [%s]),", tmp, strings.Join(pairs, ", ")))
		return exprRes{code: code, val: tmp}, nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.LogicQuery != nil:
		return c.compileLogicQuery(p.LogicQuery)
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
