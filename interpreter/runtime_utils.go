package interpreter

import (
	"fmt"
	"strconv"

	"github.com/alecthomas/participle/v2/lexer"

	"mochi/parser"
	"mochi/runtime/data"
	"mochi/types"
)

// applyBinary applies a binary operator on two generic values.
func applyBinary(pos lexer.Position, left any, op string, right any) (any, error) {
	lv := anyToValue(left)
	rv := anyToValue(right)
	res, err := applyBinaryValue(pos, lv, op, rv)
	if err != nil {
		return nil, err
	}
	return valueToAny(res), nil
}

// applyBinaryValue applies a binary operator on two Value operands.
func applyBinaryValue(pos lexer.Position, left Value, op string, right Value) (Value, error) {
	if fn, ok := binaryOps[op]; ok {
		return fn(pos, left, right)
	}
	return Value{}, errInvalidOperator(pos, op, left.Tag.String(), right.Tag.String())
}

func applyIntBinaryValue(pos lexer.Position, l int, op string, r int) (Value, error) {
	switch op {
	case "/":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagInt, Int: l / r}, nil
	case "%":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagInt, Int: l % r}, nil
	case "+":
		return Value{Tag: TagInt, Int: l + r}, nil
	case "-":
		return Value{Tag: TagInt, Int: l - r}, nil
	case "*":
		return Value{Tag: TagInt, Int: l * r}, nil
	case "==":
		return Value{Tag: TagBool, Bool: l == r}, nil
	case "!=":
		return Value{Tag: TagBool, Bool: l != r}, nil
	case "<":
		return Value{Tag: TagBool, Bool: l < r}, nil
	case "<=":
		return Value{Tag: TagBool, Bool: l <= r}, nil
	case ">":
		return Value{Tag: TagBool, Bool: l > r}, nil
	case ">=":
		return Value{Tag: TagBool, Bool: l >= r}, nil
	default:
		return Value{}, errInvalidOperator(pos, op, "int", "int")
	}
}

func applyInt64Binary(pos lexer.Position, l int64, op string, r int64) (Value, error) {
	switch op {
	case "/":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagInt, Int: int(l / r)}, nil
	case "%":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagInt, Int: int(l % r)}, nil
	case "+":
		return Value{Tag: TagInt, Int: int(l + r)}, nil
	case "-":
		return Value{Tag: TagInt, Int: int(l - r)}, nil
	case "*":
		return Value{Tag: TagInt, Int: int(l * r)}, nil
	case "==":
		return Value{Tag: TagBool, Bool: l == r}, nil
	case "!=":
		return Value{Tag: TagBool, Bool: l != r}, nil
	case "<":
		return Value{Tag: TagBool, Bool: l < r}, nil
	case "<=":
		return Value{Tag: TagBool, Bool: l <= r}, nil
	case ">":
		return Value{Tag: TagBool, Bool: l > r}, nil
	case ">=":
		return Value{Tag: TagBool, Bool: l >= r}, nil
	default:
		return Value{}, errInvalidOperator(pos, op, "int", "int")
	}
}

func applyFloatBinaryValue(pos lexer.Position, l float64, op string, r float64) (Value, error) {
	switch op {
	case "/":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagFloat, Float: l / r}, nil
	case "+":
		return Value{Tag: TagFloat, Float: l + r}, nil
	case "-":
		return Value{Tag: TagFloat, Float: l - r}, nil
	case "*":
		return Value{Tag: TagFloat, Float: l * r}, nil
	case "==":
		return Value{Tag: TagBool, Bool: l == r}, nil
	case "!=":
		return Value{Tag: TagBool, Bool: l != r}, nil
	case "<":
		return Value{Tag: TagBool, Bool: l < r}, nil
	case "<=":
		return Value{Tag: TagBool, Bool: l <= r}, nil
	case ">":
		return Value{Tag: TagBool, Bool: l > r}, nil
	case ">=":
		return Value{Tag: TagBool, Bool: l >= r}, nil
	default:
		return Value{}, errInvalidOperator(pos, op, "float", "float")
	}
}

// applyUnary applies a unary operator on a generic value.
func applyUnary(pos lexer.Position, op string, val any) (any, error) {
	v := anyToValue(val)
	res, err := applyUnaryValue(pos, op, v)
	if err != nil {
		return nil, err
	}
	return valueToAny(res), nil
}

// applyUnaryValue applies a unary operator on a Value.
func applyUnaryValue(pos lexer.Position, op string, val Value) (Value, error) {
	if fn, ok := unaryOps[op]; ok {
		return fn(pos, val)
	}
	return Value{}, errUnknownUnaryOperator(pos, op)
}

func truthy(val any) bool {
	return anyToValue(val).Truthy()
}

// execLoopBody executes a sequence of statements that form the body of a loop.
// It returns true if a `continue` statement was encountered. Any `break` or
// `return` signals are passed back to the caller as errors.
func (i *Interpreter) execLoopBody(body []*parser.Statement) (bool, error) {
	for _, s := range body {
		if err := i.evalStmt(s); err != nil {
			switch err.(type) {
			case continueSignal:
				return true, nil
			case breakSignal, returnSignal:
				return false, err
			default:
				return false, err
			}
		}
	}
	return false, nil
}

// forEach iterates over any supported collection type and invokes fn for each
// item. The callback may return a `continue` flag to skip to the next iteration.
// Errors, including break and return signals, are propagated to the caller.
func (i *Interpreter) forEach(pos lexer.Position, src any, fn func(any) (bool, error)) error {
	switch coll := src.(type) {
	case []any:
		for _, item := range coll {
			cont, err := fn(item)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case *data.Group:
		for _, item := range coll.Items {
			cont, err := fn(item)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case map[any]any:
		for k := range coll {
			cont, err := fn(k)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case map[string]any:
		for k := range coll {
			cont, err := fn(k)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case map[int]any:
		for k := range coll {
			cont, err := fn(k)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case string:
		for _, r := range coll {
			cont, err := fn(string(r))
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	default:
		return errInvalidIterator(pos, fmt.Sprintf("%T", src))
	}
	return nil
}

func castValue(pos lexer.Position, t types.Type, v any) (any, error) {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		switch x := v.(type) {
		case int:
			return x, nil
		case float64:
			return int(x), nil
		default:
			return nil, errCastType(pos, v, t)
		}
	case types.FloatType:
		switch x := v.(type) {
		case float64:
			return x, nil
		case int:
			return float64(x), nil
		default:
			return nil, errCastType(pos, v, t)
		}
	case types.StringType:
		if s, ok := v.(string); ok {
			return s, nil
		}
		return nil, errCastType(pos, v, t)
	case types.BoolType:
		if b, ok := v.(bool); ok {
			return b, nil
		}
		return nil, errCastType(pos, v, t)
	case types.ListType:
		list, ok := v.([]any)
		if !ok {
			return nil, errCastType(pos, v, t)
		}
		out := make([]any, len(list))
		for i, item := range list {
			cv, err := castValue(pos, tt.Elem, item)
			if err != nil {
				return nil, err
			}
			out[i] = cv
		}
		return out, nil
	case types.MapType:
		switch m := v.(type) {
		case map[string]any:
			out := map[string]any{}
			for k, val := range m {
				cv, err := castValue(pos, tt.Value, val)
				if err != nil {
					return nil, err
				}
				out[k] = cv
			}
			if _, ok := tt.Key.(types.StringType); ok {
				return out, nil
			}
			if _, ok := tt.Key.(types.IntType); ok {
				intMap := make(map[int]any, len(out))
				for k, v := range out {
					iv, err := strconv.Atoi(k)
					if err != nil {
						return nil, errCastType(pos, v, t)
					}
					intMap[iv] = v
				}
				return intMap, nil
			}
			return out, nil
		case map[int]any:
			out := make(map[int]any, len(m))
			for k, val := range m {
				cv, err := castValue(pos, tt.Value, val)
				if err != nil {
					return nil, err
				}
				out[k] = cv
			}
			return out, nil
		case map[any]any:
			out := make(map[any]any, len(m))
			for k, val := range m {
				cv, err := castValue(pos, tt.Value, val)
				if err != nil {
					return nil, err
				}
				out[k] = cv
			}
			return out, nil
		default:
			return nil, errCastType(pos, v, t)
		}
	case types.StructType:
		m, ok := v.(map[string]any)
		if !ok {
			return nil, errCastType(pos, v, t)
		}
		out := map[string]any{"__name": tt.Name}
		for name, ft := range tt.Fields {
			fv, ok := m[name]
			if !ok {
				return nil, errCastMissingField(pos, name, tt.Name)
			}
			cv, err := castValue(pos, ft, fv)
			if err != nil {
				return nil, err
			}
			out[name] = cv
		}
		return out, nil
	default:
		return v, nil
	}
}

func resolveTypeRef(t *parser.TypeRef, env *types.Env) types.Type {
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p, env)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return, env)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return types.ListType{Elem: resolveTypeRef(t.Generic.Args[0], env)}
			}
		case "map":
			if len(t.Generic.Args) == 2 {
				return types.MapType{Key: resolveTypeRef(t.Generic.Args[0], env), Value: resolveTypeRef(t.Generic.Args[1], env)}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if st, ok := env.GetStruct(*t.Simple); ok {
				return st
			}
			if ut, ok := env.GetUnion(*t.Simple); ok {
				return ut
			}
		}
	}
	return types.AnyType{}
}

func toAnyMap(m any) map[string]any {
	switch v := m.(type) {
	case map[string]any:
		return v
	case map[string]string:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[k] = vv
		}
		return out
	case map[int]any:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[strconv.Itoa(k)] = vv
		}
		return out
	case map[any]any:
		out := make(map[string]any, len(v))
		for kk, vv := range v {
			out[fmt.Sprint(kk)] = vv
		}
		return out
	default:
		return nil
	}
}

func toMapSlice(v any) ([]map[string]any, bool) {
	switch rows := v.(type) {
	case []map[string]any:
		return rows, true
	case []any:
		out := make([]map[string]any, len(rows))
		for i, item := range rows {
			m, ok := item.(map[string]any)
			if !ok {
				return nil, false
			}
			out[i] = m
		}
		return out, true
	default:
		return nil, false
	}
}

func simpleStringKey(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
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
