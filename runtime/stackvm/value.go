package stackvm

// ValueTag represents the type of a runtime value.
type ValueTag uint8

const (
	ValueNull ValueTag = iota
	ValueInt
	ValueFloat
	ValueStr
	ValueBool
	ValueList
	ValueMap
	ValueFunc
)

// Value is a tagged union used at runtime to avoid reflection.
type Value struct {
	Tag   ValueTag
	Int   int
	Float float64
	Str   string
	Bool  bool
	List  []Value
	Map   map[string]Value
	Func  any
}

// Truthy returns the boolean interpretation of v.
func (v Value) Truthy() bool {
	switch v.Tag {
	case ValueBool:
		return v.Bool
	case ValueInt:
		return v.Int != 0
	case ValueFloat:
		return v.Float != 0
	case ValueStr:
		return v.Str != ""
	case ValueList:
		return len(v.List) > 0
	case ValueMap:
		return len(v.Map) > 0
	default:
		return false
	}
}

func anyToValue(v any) Value {
	switch val := v.(type) {
	case nil:
		return Value{Tag: ValueNull}
	case int:
		return Value{Tag: ValueInt, Int: val}
	case int64:
		return Value{Tag: ValueInt, Int: int(val)}
	case float64:
		return Value{Tag: ValueFloat, Float: val}
	case string:
		return Value{Tag: ValueStr, Str: val}
	case bool:
		return Value{Tag: ValueBool, Bool: val}
	case []any:
		list := make([]Value, len(val))
		for i, x := range val {
			list[i] = anyToValue(x)
		}
		return Value{Tag: ValueList, List: list}
	case map[string]any:
		m := make(map[string]Value, len(val))
		for k, x := range val {
			m[k] = anyToValue(x)
		}
		return Value{Tag: ValueMap, Map: m}
	case []Value:
		return Value{Tag: ValueList, List: val}
	case map[string]Value:
		return Value{Tag: ValueMap, Map: val}
	default:
		return Value{Tag: ValueNull}
	}
}

func valueToAny(v Value) any {
	switch v.Tag {
	case ValueInt:
		return v.Int
	case ValueFloat:
		return v.Float
	case ValueStr:
		return v.Str
	case ValueBool:
		return v.Bool
	case ValueList:
		out := make([]any, len(v.List))
		for i, x := range v.List {
			out[i] = valueToAny(x)
		}
		return out
	case ValueMap:
		m := make(map[string]any, len(v.Map))
		for k, x := range v.Map {
			m[k] = valueToAny(x)
		}
		return m
	default:
		return nil
	}
}

func toFloat(v Value) float64 {
	if v.Tag == ValueFloat {
		return v.Float
	}
	return float64(v.Int)
}

func valuesEqual(a, b Value) bool {
	if a.Tag == ValueNull || b.Tag == ValueNull {
		return a.Tag == ValueNull && b.Tag == ValueNull
	}
	if a.Tag == ValueFloat || b.Tag == ValueFloat {
		return toFloat(a) == toFloat(b)
	}
	if a.Tag != b.Tag {
		return false
	}
	switch a.Tag {
	case ValueInt:
		return a.Int == b.Int
	case ValueBool:
		return a.Bool == b.Bool
	case ValueStr:
		return a.Str == b.Str
	case ValueList:
		if len(a.List) != len(b.List) {
			return false
		}
		for i := range a.List {
			if !valuesEqual(a.List[i], b.List[i]) {
				return false
			}
		}
		return true
	case ValueMap:
		if len(a.Map) != len(b.Map) {
			return false
		}
		for k, av := range a.Map {
			bv, ok := b.Map[k]
			if !ok || !valuesEqual(av, bv) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func valueLess(a, b Value) bool {
	if a.Tag == ValueNull || b.Tag == ValueNull {
		if a.Tag == ValueNull && b.Tag != ValueNull {
			return true
		}
		return false
	}
	switch a.Tag {
	case ValueInt:
		switch b.Tag {
		case ValueInt:
			return a.Int < b.Int
		case ValueFloat:
			return float64(a.Int) < b.Float
		}
	case ValueFloat:
		switch b.Tag {
		case ValueInt:
			return a.Float < float64(b.Int)
		case ValueFloat:
			return a.Float < b.Float
		}
	case ValueStr:
		if b.Tag == ValueStr {
			return a.Str < b.Str
		}
	}
	return false
}
