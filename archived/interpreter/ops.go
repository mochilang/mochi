package interpreter

import (
	"context"
	"fmt"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"

	"mochi/parser"
	"mochi/runtime/agent"
	"mochi/types"
)

// invokeClosure executes a closure with the provided arguments.
func (i *Interpreter) invokeClosure(pos lexer.Position, cl Closure, args []*parser.Expr) (any, error) {
	totalArgs := len(cl.Args) + len(args)
	fullParamCount := len(cl.FullParams)
	if totalArgs > fullParamCount {
		return nil, errTooManyFunctionArgs(pos, cl.Name, fullParamCount, totalArgs)
	}

	allArgs := append([]Value{}, cl.Args...)
	for _, a := range args {
		v, err := i.evalExpr(a)
		if err != nil {
			return nil, err
		}
		allArgs = append(allArgs, anyToValue(v))
	}

	if totalArgs < fullParamCount {
		return Closure{
			Name:       cl.Name,
			Fn:         cl.Fn,
			Env:        cl.Env,
			Args:       allArgs,
			FullParams: cl.FullParams,
		}, nil
	}

	if len(cl.FullParams) != len(allArgs) {
		return nil, errInternalClosureArgMismatch(pos)
	}

	child := types.NewEnv(cl.Env)
	for idx, param := range cl.FullParams {
		child.SetValue(param.Name, valueToAny(allArgs[idx]), true)
	}
	old := i.env
	i.env = child
	defer func() { i.env = old }()
	if cl.Fn.ExprBody != nil {
		return i.evalExpr(cl.Fn.ExprBody)
	}
	var ret any
	for _, stmt := range cl.Fn.BlockBody {
		if err := i.evalStmt(stmt); err != nil {
			if r, ok := err.(returnSignal); ok {
				ret = r.value
				err = nil
				break
			}
			return nil, err
		}
	}
	return ret, nil
}

// applyCallOp handles a postfix call operation on a value.
func (i *Interpreter) applyCallOp(val any, call *parser.CallOp) (any, error) {
	if i.ffi.IsValue(val) {
		args := make([]any, len(call.Args))
		for idx, a := range call.Args {
			v, err := i.evalExpr(a)
			if err != nil {
				return nil, err
			}
			args[idx] = v
		}
		return i.ffi.Call(val, args)
	}

	if ai, ok := val.(agentIntent); ok {
		args := make([]agent.Value, len(call.Args))
		for idx, a := range call.Args {
			v, err := i.evalExpr(a)
			if err != nil {
				return nil, err
			}
			args[idx] = v
		}
		return ai.inst.agent.Call(context.Background(), ai.decl.Name, args...)
	}

	if sm, ok := val.(stringMethod); ok {
		if sm.name == "contains" {
			if len(call.Args) != 1 {
				return nil, errTooManyFunctionArgs(call.Pos, sm.name, 1, len(call.Args))
			}
			arg, err := i.evalExpr(call.Args[0])
			if err != nil {
				return nil, err
			}
			s, ok := arg.(string)
			if !ok {
				return nil, fmt.Errorf("contains() arg must be string")
			}
			return strings.Contains(sm.recv, s), nil
		}
	}

	cl, ok := val.(Closure)
	if !ok {
		return nil, errUndefinedFunctionOrClosure(call.Pos, "")
	}
	return i.invokeClosure(call.Pos, cl, call.Args)
}

// applyIndex performs indexing or slicing on val using idx.
func (i *Interpreter) applyIndex(val any, idx *parser.IndexOp) (any, error) {
	switch src := val.(type) {
	case []any:
		if idx.Colon == nil {
			if idx.Start == nil {
				return nil, errInvalidIndex(idx.Pos, nil)
			}
			index, err := i.evalExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			n, ok := toInt(index)
			if !ok {
				return nil, errInvalidIndex(idx.Pos, index)
			}
			if n < 0 {
				n += len(src)
			}
			if n < 0 || n >= len(src) {
				return nil, errIndexOutOfBounds(idx.Pos, n, len(src))
			}
			return src[n], nil
		}
		start, end, step := 0, len(src), 1
		if idx.Start != nil {
			s, err := i.evalExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			if n, ok := toInt(s); ok {
				if n < 0 {
					n += len(src)
				}
				start = n
			} else {
				return nil, errInvalidIndex(idx.Pos, s)
			}
		}
		if idx.End != nil {
			e, err := i.evalExpr(idx.End)
			if err != nil {
				return nil, err
			}
			if n, ok := toInt(e); ok {
				if n < 0 {
					n += len(src)
				}
				end = n
			} else {
				return nil, errInvalidIndex(idx.Pos, e)
			}
		}
		if idx.Step != nil {
			st, err := i.evalExpr(idx.Step)
			if err != nil {
				return nil, err
			}
			if n, ok := toInt(st); ok {
				step = n
			} else {
				return nil, errInvalidIndex(idx.Pos, st)
			}
			if step == 0 {
				return nil, fmt.Errorf("invalid slice step")
			}
		}
		if start < 0 || end > len(src) {
			return nil, errSliceOutOfBounds(idx.Pos, start, end, len(src))
		}
		var out []any
		if step > 0 {
			if end < start {
				end = start
			}
			for j := start; j < end; j += step {
				out = append(out, src[j])
			}
		} else {
			if end > start {
				end = start
			}
			for j := start; j > end; j += step {
				out = append(out, src[j])
			}
		}
		return out, nil

	case string:
		runes := []rune(src)
		if idx.Colon == nil {
			if idx.Start == nil {
				return nil, errInvalidIndex(idx.Pos, nil)
			}
			index, err := i.evalExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			n, ok := toInt(index)
			if !ok {
				return nil, errInvalidIndex(idx.Pos, index)
			}
			if n < 0 {
				n += len(runes)
			}
			if n < 0 || n >= len(runes) {
				return nil, errIndexOutOfBounds(idx.Pos, n, len(runes))
			}
			return string(runes[n]), nil
		}
		start, end, step := 0, len(runes), 1
		if idx.Start != nil {
			s, err := i.evalExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			if n, ok := toInt(s); ok {
				if n < 0 {
					n += len(runes)
				}
				start = n
			} else {
				return nil, errInvalidIndex(idx.Pos, s)
			}
		}
		if idx.End != nil {
			e, err := i.evalExpr(idx.End)
			if err != nil {
				return nil, err
			}
			if n, ok := toInt(e); ok {
				if n < 0 {
					n += len(runes)
				}
				end = n
			} else {
				return nil, errInvalidIndex(idx.Pos, e)
			}
		}
		if idx.Step != nil {
			st, err := i.evalExpr(idx.Step)
			if err != nil {
				return nil, err
			}
			if n, ok := toInt(st); ok {
				step = n
			} else {
				return nil, errInvalidIndex(idx.Pos, st)
			}
			if step == 0 {
				return nil, fmt.Errorf("invalid slice step")
			}
		}
		if start < 0 || end > len(runes) {
			return nil, errSliceOutOfBounds(idx.Pos, start, end, len(runes))
		}
		var res []rune
		if step > 0 {
			if end < start {
				end = start
			}
			for j := start; j < end; j += step {
				res = append(res, runes[j])
			}
		} else {
			if end > start {
				end = start
			}
			for j := start; j > end; j += step {
				res = append(res, runes[j])
			}
		}
		return string(res), nil

	case map[string]any:
		if idx.Colon != nil {
			return nil, errInvalidIndexTarget(idx.Pos, "map")
		}
		if idx.Start == nil {
			return nil, errInvalidIndex(idx.Pos, nil)
		}
		key, err := i.evalExpr(idx.Start)
		if err != nil {
			return nil, err
		}
		k, ok := key.(string)
		if !ok {
			return nil, errInvalidMapKey(idx.Pos, key)
		}
		return src[k], nil
	case map[int]any:
		if idx.Colon != nil {
			return nil, errInvalidIndexTarget(idx.Pos, "map")
		}
		if idx.Start == nil {
			return nil, errInvalidIndex(idx.Pos, nil)
		}
		key, err := i.evalExpr(idx.Start)
		if err != nil {
			return nil, err
		}
		var k int
		switch v := key.(type) {
		case int:
			k = v
		case int64:
			k = int(v)
		default:
			return nil, errInvalidMapKey(idx.Pos, key)
		}
		return src[k], nil
	case map[any]any:
		if idx.Colon != nil {
			return nil, errInvalidIndexTarget(idx.Pos, "map")
		}
		if idx.Start == nil {
			return nil, errInvalidIndex(idx.Pos, nil)
		}
		key, err := i.evalExpr(idx.Start)
		if err != nil {
			return nil, err
		}
		switch key.(type) {
		case string, int, int64:
			if iv, ok := key.(int64); ok {
				key = int(iv)
			}
			return src[key], nil
		default:
			return nil, errInvalidMapKey(idx.Pos, key)
		}

	default:
		return nil, errInvalidIndexTarget(idx.Pos, fmt.Sprintf("%T", src))
	}
}

// applyCast casts val to the target type described by cast.
func (i *Interpreter) applyCast(val any, cast *parser.CastOp) (any, error) {
	typ := resolveTypeRef(cast.Type, i.types)
	return castValue(cast.Pos, typ, val)
}
