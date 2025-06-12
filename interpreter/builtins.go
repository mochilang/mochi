package interpreter

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"mochi/parser"
	"mochi/runtime/data"
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
		"print": builtinPrint,
		"len":   builtinLen,
		"now":   builtinNow,
		"json":  builtinJSON,
		"str":   builtinStr,
		"count": builtinCount,
		"avg":   builtinAvg,
	}
}
