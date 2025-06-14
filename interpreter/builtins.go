package interpreter

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"mochi/parser"
	"mochi/runtime/data"
	"mochi/runtime/datalog"
	"mochi/types"
)

// builtinPrint implements the print(...) function.
func builtinPrint(i *Interpreter, c *parser.CallExpr) (any, error) {
	var sb strings.Builder
	for _, arg := range c.Args {
		val, err := i.evalExpr(arg)
		if err != nil {
			return nil, err
		}
		fmt.Fprintf(&sb, "%v ", val)
	}
	_, err := fmt.Fprintln(i.env.Writer(), strings.TrimSpace(sb.String()))
	return nil, err
}

// builtinLen implements len(x).
func builtinLen(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, errTooManyFunctionArgs(c.Pos, "len", 1, len(c.Args))
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	switch v := val.(type) {
	case []any:
		return len(v), nil
	case string:
		return len([]rune(v)), nil
	case map[string]any:
		return len(v), nil
	case map[int]any:
		return len(v), nil
	case map[any]any:
		return len(v), nil
	default:
		return nil, errInvalidLenOperand(c.Pos, fmt.Sprintf("%T", val))
	}
}

func builtinNow(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 0 {
		return nil, fmt.Errorf("now() takes no arguments")
	}
	return time.Now().UnixNano(), nil
}

func builtinJSON(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, fmt.Errorf("json(x) takes exactly one argument")
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	data, err := json.MarshalIndent(val, "", "  ")
	if err != nil {
		return nil, err
	}
	_, err = fmt.Fprintln(i.env.Writer(), string(data))
	return nil, err
}

func builtinStr(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, fmt.Errorf("str(x) takes exactly one argument")
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	return fmt.Sprint(val), nil
}

// builtinEval implements eval(code).
func builtinEval(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, fmt.Errorf("eval(code) takes exactly one argument")
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	src, ok := val.(string)
	if !ok {
		return nil, fmt.Errorf("eval() expects a string argument")
	}

	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}

	if errs := types.Check(prog, i.env); len(errs) > 0 {
		return nil, errs[0]
	}

	// If the program is a single expression statement, just evaluate it and
	// return the result.
	if len(prog.Statements) == 1 && prog.Statements[0].Expr != nil {
		return i.evalExpr(prog.Statements[0].Expr.Expr)
	}

	var result any
	for idx, stmt := range prog.Statements {
		if stmt.Expr != nil && idx == len(prog.Statements)-1 {
			result, err = i.evalExpr(stmt.Expr.Expr)
		} else {
			err = i.evalStmt(stmt)
		}
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func builtinCount(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, fmt.Errorf("count(x) takes exactly one argument")
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	switch v := val.(type) {
	case []any:
		return len(v), nil
	case *data.Group:
		return len(v.Items), nil
	default:
		return nil, fmt.Errorf("count() expects list or group, got %T", val)
	}
}

func builtinAvg(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, fmt.Errorf("avg(x) takes exactly one argument")
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	var list []any
	switch v := val.(type) {
	case []any:
		list = v
	case *data.Group:
		list = v.Items
	default:
		return nil, fmt.Errorf("avg() expects list or group, got %T", val)
	}
	if len(list) == 0 {
		return 0, nil
	}
	var sum float64
	for _, it := range list {
		switch n := it.(type) {
		case int:
			sum += float64(n)
		case int64:
			sum += float64(n)
		case float64:
			sum += n
		default:
			return nil, fmt.Errorf("avg() expects numbers, got %T", it)
		}
	}
	return sum / float64(len(list)), nil
}

func builtinFact(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) < 2 {
		return nil, fmt.Errorf("fact(name, args...) requires at least one argument")
	}
	nameVal, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	name, ok := nameVal.(string)
	if !ok {
		return nil, fmt.Errorf("fact name must be string")
	}
	args := make([]any, 0, len(c.Args)-1)
	for _, a := range c.Args[1:] {
		v, err := i.evalExpr(a)
		if err != nil {
			return nil, err
		}
		args = append(args, v)
	}
	i.datalog.AddFact(name, args)
	return nil, nil
}

func builtinRule(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, fmt.Errorf("rule() expects a single string argument")
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	s, ok := val.(string)
	if !ok {
		return nil, fmt.Errorf("rule() expects a string")
	}
	r, err := datalog.ParseRule(s)
	if err != nil {
		return nil, err
	}
	i.datalog.AddRule(r)
	return nil, nil
}

func builtinQuery(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, fmt.Errorf("query() expects a single string argument")
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	s, ok := val.(string)
	if !ok {
		return nil, fmt.Errorf("query() expects a string")
	}
	q, err := datalog.ParseQuery(s)
	if err != nil {
		return nil, err
	}
	res, err := i.datalog.Query(q)
	if err != nil {
		return nil, err
	}
	out := make([]any, len(res))
	for idx, m := range res {
		out[idx] = m
	}
	return out, nil
}

func (i *Interpreter) builtinFuncs() map[string]func(*Interpreter, *parser.CallExpr) (any, error) {
	return map[string]func(*Interpreter, *parser.CallExpr) (any, error){
		"print": builtinPrint,
		"len":   builtinLen,
		"now":   builtinNow,
		"json":  builtinJSON,
		"str":   builtinStr,
		"count": builtinCount,
		"avg":   builtinAvg,
		"eval":  builtinEval,
		"fact":  builtinFact,
		"rule":  builtinRule,
		"query": builtinQuery,
	}
}
