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
