package types

import (
	"mochi/parser"
)

// CheckExprType returns the static type of expression e using the full type
// checker. If type checking fails the result is AnyType.
func CheckExprType(e *parser.Expr, env *Env) Type {
	if e == nil {
		return AnyType{}
	}
	prog := &parser.Program{Statements: []*parser.Statement{{Let: &parser.LetStmt{Name: "_tmp", Value: e}}}}
	child := NewEnv(env)
	if errs := Check(prog, child); len(errs) == 0 {
		if t, err := child.GetVar("_tmp"); err == nil {
			return t
		}
	}
	return AnyType{}
}

// ContainsAny reports whether t or any of its components is the Any type.
func ContainsAny(t Type) bool {
	switch tt := t.(type) {
	case AnyType:
		return true
	case ListType:
		return ContainsAny(tt.Elem)
	case MapType:
		return ContainsAny(tt.Key) || ContainsAny(tt.Value)
	}
	return false
}
