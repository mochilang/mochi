package cobol

import (
	"encoding/json"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

// evalInt evaluates an expression tree to an integer if possible.
func evalInt(e Expr) (int, bool) {
	switch v := e.(type) {
	case *IntLit:
		return v.Value, true
	case *UnaryExpr:
		if strings.TrimSpace(v.Op) == "-" {
			if n, ok := evalInt(v.Expr); ok {
				return -n, true
			}
		}
	case *BinaryExpr:
		a, ok1 := evalInt(v.Left)
		b, ok2 := evalInt(v.Right)
		if ok1 && ok2 {
			switch v.Op {
			case "+":
				return a + b, true
			case "-":
				return a - b, true
			case "*":
				return a * b, true
			case "/":
				if b != 0 {
					return a / b, true
				}
			case "%":
				if b != 0 {
					return a % b, true
				}
			}
		}
	}
	return 0, false
}

// evalQueryString runs a query using the interpreter and returns the
// formatted string result. It relies only on constVars for inputs.
func evalQueryString(q *parser.QueryExpr) (string, error) {
	env := types.NewEnv(nil)
	for name, val := range constVars {
		env.SetVar(name, types.AnyType{}, false)
		env.SetValue(name, val, false)
	}
	prog := &parser.Program{}
	interp := interpreter.New(prog, env, ".")
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Query: q}}}}}
	val, err := interp.EvalExpr(expr)
	if err != nil {
		return "", err
	}
	switch v := val.(type) {
	case []any:
		items := make([]map[string]any, len(v))
		for i, it := range v {
			if m, ok := it.(map[string]any); ok {
				items[i] = m
			} else {
				b, _ := json.Marshal(it)
				return string(b), nil
			}
		}
		return formatConstMapSlice(items), nil
	case map[string]any:
		ms := []map[string]any{v}
		return formatConstMapSlice(ms), nil
	default:
		b, _ := json.Marshal(v)
		s := string(b)
		s = strings.ReplaceAll(s, ":", ": ")
		s = strings.ReplaceAll(s, ",", ", ")
		return s, nil
	}
}

func formatConstMap(m map[interface{}]interface{}) string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, fmt.Sprintf("%v", k))
	}
	sort.Strings(keys)
	parts := make([]string, len(keys))
	for i, kstr := range keys {
		var key interface{}
		if iv, err := strconv.Atoi(kstr); err == nil {
			key = iv
		} else {
			key = kstr
		}
		val := m[key]
		switch v := val.(type) {
		case string:
			parts[i] = fmt.Sprintf("%q: %s", kstr, v)
		case int:
			parts[i] = fmt.Sprintf("%q: %d", kstr, v)
		default:
			parts[i] = fmt.Sprintf("%q: %v", kstr, v)
		}
	}
	return "{" + strings.Join(parts, ", ") + "}"
}

func formatConstMapSlice(ms []map[string]any) string {
	parts := make([]string, len(ms))
	for i, m := range ms {
		im := make(map[interface{}]interface{}, len(m))
		for k, v := range m {
			im[k] = v
		}
		parts[i] = formatConstMap(im)
	}
	return strings.Join(parts, " ")
}

func formatConstValues(m map[interface{}]interface{}) string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, fmt.Sprintf("%v", k))
	}
	sort.Strings(keys)
	vals := make([]string, len(keys))
	for i, kstr := range keys {
		var key interface{}
		if iv, err := strconv.Atoi(kstr); err == nil {
			key = iv
		} else {
			key = kstr
		}
		val := m[key]
		switch v := val.(type) {
		case string:
			vals[i] = v
		case int:
			vals[i] = fmt.Sprintf("%d", v)
		default:
			vals[i] = fmt.Sprintf("%v", v)
		}
	}
	return "[" + strings.Join(vals, ", ") + "]"
}
