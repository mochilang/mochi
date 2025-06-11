package interpreter

import (
	"context"
	"fmt"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"

	"mochi/parser"
	"mochi/runtime/agent"
	denoffi "mochi/runtime/ffi/deno"
	goffi "mochi/runtime/ffi/go"
	pythonffi "mochi/runtime/ffi/python"
	"mochi/types"
)

// invokeClosure executes a closure with the provided arguments.
func (i *Interpreter) invokeClosure(pos lexer.Position, cl closure, args []*parser.Expr) (any, error) {
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
		return closure{
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
	if pv, ok := val.(pythonValue); ok {
		args := make([]any, len(call.Args))
		for idx, a := range call.Args {
			v, err := i.evalExpr(a)
			if err != nil {
				return nil, err
			}
			args[idx] = v
		}
		return pythonffi.Attr(pv.module, strings.Join(pv.attrs, "."), args...)
	}

	if gv, ok := val.(goValue); ok {
		args := make([]any, len(call.Args))
		for idx, a := range call.Args {
			v, err := i.evalExpr(a)
			if err != nil {
				return nil, err
			}
			args[idx] = v
		}
		name := gv.module
		if len(gv.attrs) > 0 {
			name += "." + strings.Join(gv.attrs, ".")
		}
		return goffi.Call(name, args...)
	}

	if tv, ok := val.(tsValue); ok {
		args := make([]any, len(call.Args))
		for idx, a := range call.Args {
			v, err := i.evalExpr(a)
			if err != nil {
				return nil, err
			}
			args[idx] = v
		}
		return denoffi.Attr(tv.module, strings.Join(tv.attrs, ":"), args...)
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

	cl, ok := val.(closure)
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
			n, ok := index.(int)
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
		start, end := 0, len(src)
		if idx.Start != nil {
			s, err := i.evalExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			if n, ok := s.(int); ok {
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
			if n, ok := e.(int); ok {
				if n < 0 {
					n += len(src)
				}
				end = n
			} else {
				return nil, errInvalidIndex(idx.Pos, e)
			}
		}
		if start < 0 || end > len(src) || start > end {
			return nil, errSliceOutOfBounds(idx.Pos, start, end, len(src))
		}
		return src[start:end], nil

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
			n, ok := index.(int)
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
		start, end := 0, len(runes)
		if idx.Start != nil {
			s, err := i.evalExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			if n, ok := s.(int); ok {
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
			if n, ok := e.(int); ok {
				if n < 0 {
					n += len(runes)
				}
				end = n
			} else {
				return nil, errInvalidIndex(idx.Pos, e)
			}
		}
		if start < 0 || end > len(runes) || start > end {
			return nil, errSliceOutOfBounds(idx.Pos, start, end, len(runes))
		}
		return string(runes[start:end]), nil

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
		keyStr, ok := key.(string)
		if !ok {
			return nil, errInvalidMapKey(idx.Pos, key)
		}
		return src[keyStr], nil

	default:
		return nil, errInvalidIndexTarget(idx.Pos, fmt.Sprintf("%T", src))
	}
}

// applyCast casts val to the target type described by cast.
func (i *Interpreter) applyCast(val any, cast *parser.CastOp) (any, error) {
	typ := resolveTypeRef(cast.Type, i.types)
	return castValue(cast.Pos, typ, val)
}
