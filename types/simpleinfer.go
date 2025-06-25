package types

import (
	"strings"

	"mochi/parser"
)

// TypeOfExpr performs a minimal type inference on e using env.
// It distinguishes literals, lists, maps and known variable references.
func TypeOfExpr(e *parser.Expr, env *Env) Type {
	if e == nil {
		return AnyType{}
	}
	if id, ok := exprIdent(e); ok && env != nil {
		if t, err := env.GetVar(id); err == nil {
			return t
		}
	}
	return TypeOfUnary(e.Binary.Left, env)
}

// TypeOfPostfix returns the type of p using minimal inference.
func TypeOfPostfix(p *parser.PostfixExpr, env *Env) Type {
	if p == nil {
		return AnyType{}
	}
	t := TypeOfPrimary(p.Target, env)
	for _, op := range p.Ops {
		if op.Cast != nil {
			t = ResolveTypeRef(op.Cast.Type, env)
		} else if op.Index != nil && op.Index.Colon == nil {
			switch tt := t.(type) {
			case ListType:
				t = tt.Elem
			case MapType:
				t = tt.Value
			case StringType:
				t = StringType{}
			default:
				t = AnyType{}
			}
		} else if op.Index != nil {
			switch tt := t.(type) {
			case ListType:
				t = tt
			case StringType:
				t = StringType{}
			default:
				t = AnyType{}
			}
		} else if op.Call != nil {
			if ft, ok := t.(FuncType); ok {
				t = ft.Return
			} else {
				t = AnyType{}
			}
		}
	}
	return t
}

// TypeOfUnary returns the type of u using minimal inference.
func TypeOfUnary(u *parser.Unary, env *Env) Type {
	if u == nil {
		return AnyType{}
	}
	return TypeOfPostfix(u.Value, env)
}

// TypeOfPrimary returns the type of primary expression p using minimal inference.
func TypeOfPrimary(p *parser.Primary, env *Env) Type {
	if p == nil {
		return AnyType{}
	}
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Str != nil:
			return StringType{}
		case p.Lit.Int != nil:
			return IntType{}
		case p.Lit.Float != nil:
			return FloatType{}
		case p.Lit.Bool != nil:
			return BoolType{}
		}
	case p.List != nil:
		return ListType{Elem: AnyType{}}
	case p.Map != nil:
		return MapType{Key: AnyType{}, Value: AnyType{}}
	case p.Struct != nil:
		if env != nil {
			if st, ok := env.GetStruct(p.Struct.Name); ok {
				return st
			}
		}
		return AnyType{}
	case p.Selector != nil:
		if env != nil {
			if len(p.Selector.Tail) > 0 {
				full := p.Selector.Root + "." + strings.Join(p.Selector.Tail, ".")
				if t, err := env.GetVar(full); err == nil {
					return t
				}
			}
			if t, err := env.GetVar(p.Selector.Root); err == nil {
				return t
			}
		}
	}
	return AnyType{}
}

// IsListType reports whether t is a ListType.
func IsListType(t Type) bool { _, ok := t.(ListType); return ok }

// IsMapType reports whether t is a MapType.
func IsMapType(t Type) bool { _, ok := t.(MapType); return ok }

// IsStringType reports whether t is a StringType.
func IsStringType(t Type) bool { _, ok := t.(StringType); return ok }

func exprIdent(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
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
	return "", false
}
