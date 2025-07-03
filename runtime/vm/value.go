package vm

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

// valueEqual reports whether two Values are identical.
func valueEqual(a, b Value) bool {
	if a.Tag != b.Tag {
		return false
	}
	switch a.Tag {
	case ValueInt:
		return a.Int == b.Int
	case ValueFloat:
		return a.Float == b.Float
	case ValueStr:
		return a.Str == b.Str
	case ValueBool:
		return a.Bool == b.Bool
	case ValueNull:
		return true
	case ValueList:
		if len(a.List) != len(b.List) {
			return false
		}
		for i := range a.List {
			if !valueEqual(a.List[i], b.List[i]) {
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
			if !ok || !valueEqual(av, bv) {
				return false
			}
		}
		return true
	default:
		return false
	}
}
