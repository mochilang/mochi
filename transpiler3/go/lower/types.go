package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
)

// lowerType maps an aotir.Type to its Go type-expression text.
// Phase 2 covers the four scalars; later phases extend the
// switch to list / map / record / agent / chan / stream /
// future / sub / sum-union / fun types.
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
		return "", fmt.Errorf("transpiler3/go/lower: Phase 2 does not handle type %s", t)
	}
}
