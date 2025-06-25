package types

import (
	"strings"

	"mochi/parser"
)

// TypeOfExprBasic performs lightweight type inference for dynamic backends.
// It distinguishes literals, lists, maps and known variable references.
func TypeOfExprBasic(e *parser.Expr, env *Env) Type {
	if e == nil {
		return AnyType{}
	}
	if id, ok := identName(e); ok && env != nil {
		if t, err := env.GetVar(id); err == nil {
			return t
		}
	}
	return TypeOfPostfixBasic(e.Binary.Left, env)
}

// TypeOfPostfixBasic infers the type of a unary expression using simple rules.
func TypeOfPostfixBasic(u *parser.Unary, env *Env) Type {
	if u == nil {
		return AnyType{}
	}
	t := TypeOfPrimaryBasic(u.Value.Target, env)
	for _, op := range u.Value.Ops {
		if op.Cast != nil {
			t = ResolveTypeRef(op.Cast.Type, env)
		}
	}
	return t
}

// TypeOfPrimaryBasic infers the type of a primary expression using simple rules.
func TypeOfPrimaryBasic(p *parser.Primary, env *Env) Type {
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

// IsListType reports whether t is a list type.
func IsListType(t Type) bool { _, ok := t.(ListType); return ok }

// IsMapType reports whether t is a map type.
func IsMapType(t Type) bool { _, ok := t.(MapType); return ok }

// IsStringType reports whether t is a string type.
func IsStringType(t Type) bool { _, ok := t.(StringType); return ok }
