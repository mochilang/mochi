package interpreter

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"time"

	"mochi/parser"
	"mochi/runtime/data"
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

func builtinAppend(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 2 {
		return nil, fmt.Errorf("append(list, x) takes exactly two arguments")
	}
	lst, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	elem, err := i.evalExpr(c.Args[1])
	if err != nil {
		return nil, err
	}
	switch v := lst.(type) {
	case []any:
		return append(v, elem), nil
	default:
		return nil, fmt.Errorf("append() expects list, got %T", lst)
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

// builtinInput reads a line from standard input.
func builtinInput(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 0 {
		return nil, fmt.Errorf("input() takes no arguments")
	}
	r, ok := i.env.Reader().(*bufio.Reader)
	if !ok {
		r = bufio.NewReader(i.env.Reader())
		i.env.SetReader(r)
	}
	line, err := r.ReadString('\n')
	if err != nil && err != io.EOF {
		return nil, err
	}
	return strings.TrimRight(line, "\r\n"), nil
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

func (i *Interpreter) builtinFuncs() map[string]func(*Interpreter, *parser.CallExpr) (any, error) {
	return map[string]func(*Interpreter, *parser.CallExpr) (any, error){
		"print":  builtinPrint,
		"len":    builtinLen,
		"append": builtinAppend,
		"now":    builtinNow,
		"json":   builtinJSON,
		"str":    builtinStr,
		"input":  builtinInput,
		"count":  builtinCount,
		"avg":    builtinAvg,
		"eval":   builtinEval,
	}
}
