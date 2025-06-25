package types

import "mochi/parser"

// IsStringExpr reports whether e is a string literal or a variable of string type.
func IsStringExpr(e *parser.Expr, env *Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(StringType); ok {
				return true
			}
		}
	}
	return false
}

// IsListExpr reports whether e is a list literal or a variable of list type.
func IsListExpr(e *parser.Expr, env *Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.List != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(ListType); ok {
				return true
			}
		}
	}
	return false
}

// IsFloatExpr reports whether e is a float literal or a variable of float type.
func IsFloatExpr(e *parser.Expr, env *Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Float != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(FloatType); ok {
				return true
			}
		}
	}
	return false
}

// IsBoolExpr reports whether e is a bool literal or a variable of bool type.
func IsBoolExpr(e *parser.Expr, env *Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Bool != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(BoolType); ok {
				return true
			}
		}
	}
	return false
}

// IsMapExpr reports whether e is a map literal or a variable of map type.
func IsMapExpr(e *parser.Expr, env *Env) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	p := e.Binary.Left.Value.Target
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(MapType); ok {
				return true
			}
		}
	}
	return false
}

// IsMapPrimary reports whether p is a map literal or variable of map type.
func IsMapPrimary(p *parser.Primary, env *Env) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(MapType); ok {
				return true
			}
		}
	}
	return false
}

// IsStringPrimary reports whether p is a string literal or variable of string type.
func IsStringPrimary(p *parser.Primary, env *Env) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(StringType); ok {
				return true
			}
		}
	}
	return false
}

// IsListPrimary reports whether p is a list literal or variable of list type.
func IsListPrimary(p *parser.Primary, env *Env) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		return true
	}
	if p.Selector != nil && env != nil {
		if typ, err := env.GetVar(p.Selector.Root); err == nil {
			if _, ok := typ.(ListType); ok {
				return true
			}
		}
	}
	return false
}
