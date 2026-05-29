package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
)

// lowerType maps an aotir.Type to its Go type-expression text.
// Phase 2 covers the four scalars; Phase 3.1 adds list<T> for
// scalar T. Later phases extend the switch to map / record /
// agent / chan / stream / future / sub / sum-union / fun types.
//
// The integer pin is `int64` (never bare `int`) and the float
// pin is `float64`, per MEP-54 §6 "Type lowering".
func (l *lowerer) lowerType(t aotir.Type) (string, error) {
	switch t {
	case aotir.TypeUnit:
		return "", nil
	case aotir.TypeString:
		return "string", nil
	case aotir.TypeInt:
		return "int64", nil
	case aotir.TypeFloat:
		return "float64", nil
	case aotir.TypeBool:
		return "bool", nil
	default:
		return "", fmt.Errorf("transpiler3/go/lower: does not handle type %s", t)
	}
}

// lowerListType produces the Go type expression `[]Elem` for a
// list<elem> value. Phase 3.1 only accepts the four scalar
// element types.
func (l *lowerer) lowerListType(elem aotir.Type) (string, error) {
	et, err := l.lowerType(elem)
	if err != nil {
		return "", fmt.Errorf("list element: %w", err)
	}
	if et == "" {
		return "", fmt.Errorf("transpiler3/go/lower: list element type cannot be unit")
	}
	return "[]" + et, nil
}

// lowerMapType produces the Go type expression `map[K]V` for a
// map<K, V> value. Phase 3.2 accepts the eight (K, V) scalar
// combinations enumerated by the verifier.
func (l *lowerer) lowerMapType(key, value aotir.Type) (string, error) {
	kt, err := l.lowerType(key)
	if err != nil {
		return "", fmt.Errorf("map key: %w", err)
	}
	vt, err := l.lowerType(value)
	if err != nil {
		return "", fmt.Errorf("map value: %w", err)
	}
	if kt == "" || vt == "" {
		return "", fmt.Errorf("transpiler3/go/lower: map key/value type cannot be unit")
	}
	return "map[" + kt + "]" + vt, nil
}

// lowerSetType produces `map[Elem]struct{}` for a set<elem> value.
// Phase 3.3 uses the idiomatic Go set encoding.
func (l *lowerer) lowerSetType(elem aotir.Type) (string, error) {
	et, err := l.lowerType(elem)
	if err != nil {
		return "", fmt.Errorf("set element: %w", err)
	}
	if et == "" {
		return "", fmt.Errorf("transpiler3/go/lower: set element type cannot be unit")
	}
	return "map[" + et + "]struct{}", nil
}
