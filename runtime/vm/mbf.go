package vm

import (
	"fmt"

	"mochi/interpreter"
	"mochi/runtime/bmon"
)

// MBF (Mochi Bytecode Format) is a compact RESP3 inspired encoding for VM bytecode.

// MarshalMBF encodes p into MBF bytes.
func MarshalMBF(p *Program) ([]byte, error) {
	return bmon.Marshal(programToAny(p))
}

// UnmarshalMBF decodes MBF bytes into a Program.
func UnmarshalMBF(data []byte) (*Program, error) {
	var v any
	if err := bmon.Unmarshal(data, &v); err != nil {
		return nil, err
	}
	return anyToProgram(v)
}

func programToAny(p *Program) any {
	funcs := make([]any, len(p.Funcs))
	for i, fn := range p.Funcs {
		instrs := make([]any, len(fn.Code))
		for j, ins := range fn.Code {
			instrs[j] = []any{
				int64(ins.Op),
				int64(ins.A),
				int64(ins.B),
				int64(ins.C),
				int64(ins.D),
				valueToAny(ins.Val),
				int64(ins.Line),
			}
		}
		funcs[i] = []any{
			fn.Name,
			int64(fn.NumRegs),
			int64(fn.Line),
			instrs,
		}
	}
	return funcs
}

func anyToProgram(v any) (*Program, error) {
	arr, ok := v.([]any)
	if !ok {
		return nil, fmt.Errorf("invalid program root: %T", v)
	}
	p := &Program{Funcs: make([]Function, len(arr))}
	for i, fv := range arr {
		fnArr, ok := fv.([]any)
		if !ok || len(fnArr) != 4 {
			return nil, fmt.Errorf("invalid function entry")
		}
		name, _ := fnArr[0].(string)
		numRegs, err := anyToInt(fnArr[1])
		if err != nil {
			return nil, err
		}
		line, err := anyToInt(fnArr[2])
		if err != nil {
			return nil, err
		}
		instrVals, ok := fnArr[3].([]any)
		if !ok {
			return nil, fmt.Errorf("invalid instrs")
		}
		fn := Function{Name: name, NumRegs: numRegs, Line: line, Code: make([]Instr, len(instrVals))}
		for j, iv := range instrVals {
			ia, ok := iv.([]any)
			if !ok || len(ia) != 7 {
				return nil, fmt.Errorf("invalid instr")
			}
			opInt, err := anyToInt(ia[0])
			if err != nil {
				return nil, err
			}
			a, err := anyToInt(ia[1])
			if err != nil {
				return nil, err
			}
			b, err := anyToInt(ia[2])
			if err != nil {
				return nil, err
			}
			c, err := anyToInt(ia[3])
			if err != nil {
				return nil, err
			}
			d, err := anyToInt(ia[4])
			if err != nil {
				return nil, err
			}
			val := anyToValueSimple(ia[5])
			line2, err := anyToInt(ia[6])
			if err != nil {
				return nil, err
			}
			fn.Code[j] = Instr{Op: Op(opInt), A: a, B: b, C: c, D: d, Val: val, Line: line2}
		}
		p.Funcs[i] = fn
	}
	return p, nil
}

func anyToInt(v any) (int, error) {
	switch n := v.(type) {
	case int64:
		return int(n), nil
	case int:
		return n, nil
	case float64:
		return int(n), nil
	default:
		return 0, fmt.Errorf("expected int, got %T", v)
	}
}

func anyToValueSimple(v any) Value {
	switch val := v.(type) {
	case nil:
		return Value{}
	case int64:
		return Value{Tag: interpreter.TagInt, Int: int(val)}
	case int:
		return Value{Tag: interpreter.TagInt, Int: val}
	case float64:
		return Value{Tag: interpreter.TagFloat, Float: val}
	case string:
		return Value{Tag: interpreter.TagStr, Str: val}
	case bool:
		return Value{Tag: interpreter.TagBool, Bool: val}
	case []any:
		list := make([]Value, len(val))
		for i, x := range val {
			list[i] = anyToValueSimple(x)
		}
		return Value{Tag: interpreter.TagList, List: list}
	case map[string]any:
		m := make(map[string]Value, len(val))
		for k, x := range val {
			m[k] = anyToValueSimple(x)
		}
		return Value{Tag: interpreter.TagMap, Map: m}
	default:
		return Value{}
	}
}
