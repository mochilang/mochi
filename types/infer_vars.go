package types

import "mochi/parser"

// IsListLiteral reports whether e represents a list literal.
func IsListLiteral(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		v := e.Binary.Left.Value
		if len(v.Ops) == 0 && v.Target != nil && v.Target.List != nil {
			return true
		}
	}
	return false
}

// IsEmptyListLiteral reports whether e is an empty list literal.
func IsEmptyListLiteral(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil && e.Binary.Left.Value != nil {
		v := e.Binary.Left.Value
		if len(v.Ops) == 0 && v.Target != nil && v.Target.List != nil && len(v.Target.List.Elems) == 0 {
			return true
		}
	}
	return false
}

// IsListExprVars reports whether e is a list expression using the provided variable sets.
// sanitize is applied to selector and function names before lookup.
func IsListExprVars(e *parser.Expr, sanitize func(string) string, listVars, funList map[string]bool) bool {
	if e == nil {
		return false
	}
	if IsListLiteral(e) || IsEmptyListLiteral(e) {
		return true
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 0 {
			if u.Value.Target != nil {
				if u.Value.Target.Selector != nil {
					name := sanitize(u.Value.Target.Selector.Root)
					if listVars[name] {
						return true
					}
				}
				if u.Value.Target.Call != nil {
					name := sanitize(u.Value.Target.Call.Func)
					if name == "append" {
						return true
					}
					if funList[name] {
						return true
					}
				}
			}
		}
	}
	return false
}

// IsStringExprVars reports whether e is a string expression using the provided variable sets.
func IsStringExprVars(e *parser.Expr, sanitize func(string) string, stringVars, funStr map[string]bool) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			v := u.Value
			if v == nil {
				return false
			}
			if len(v.Ops) == 0 {
				if v.Target != nil {
					if v.Target.Lit != nil && v.Target.Lit.Str != nil {
						return true
					}
					if v.Target.Fetch != nil {
						return true
					}
					if v.Target.Selector != nil {
						name := sanitize(v.Target.Selector.Root)
						if stringVars[name] {
							return true
						}
					}
					if v.Target.Call != nil {
						name := sanitize(v.Target.Call.Func)
						if funStr[name] {
							return true
						}
					}
					if v.Target.If != nil {
						if IsStringIfExprVars(v.Target.If, sanitize, stringVars, funStr) {
							return true
						}
					}
				}
				if v.Target != nil && v.Target.Group != nil {
					return IsStringExprVars(v.Target.Group, sanitize, stringVars, funStr)
				}
			}
		}
	}
	return false
}

// IsStringIfExprVars reports whether the if-expression returns a string.
func IsStringIfExprVars(ie *parser.IfExpr, sanitize func(string) string, stringVars, funStr map[string]bool) bool {
	if ie == nil {
		return false
	}
	if !IsStringExprVars(ie.Then, sanitize, stringVars, funStr) {
		return false
	}
	if ie.ElseIf != nil {
		return IsStringIfExprVars(ie.ElseIf, sanitize, stringVars, funStr)
	}
	if ie.Else != nil {
		return IsStringExprVars(ie.Else, sanitize, stringVars, funStr)
	}
	return false
}

// IsFloatExprVars reports whether e is a float expression using the provided sets.
func IsFloatExprVars(e *parser.Expr, sanitize func(string) string, floatVars, funFloat map[string]bool) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if IsFloatUnaryVars(e.Binary.Left, sanitize, floatVars, funFloat) {
		return true
	}
        for _, part := range e.Binary.Right {
                if IsFloatUnaryVars(part.Right, sanitize, floatVars, funFloat) {
                        return true
                }
        }
	return false
}

func IsFloatUnaryVars(u *parser.Unary, sanitize func(string) string, floatVars, funFloat map[string]bool) bool {
	if u == nil {
		return false
	}
	if IsFloatPostfixVars(u.Value, sanitize, floatVars, funFloat) {
		return true
	}
	return false
}

func IsFloatPostfixVars(p *parser.PostfixExpr, sanitize func(string) string, floatVars, funFloat map[string]bool) bool {
	if p == nil {
		return false
	}
	if IsFloatPrimaryVars(p.Target, sanitize, floatVars, funFloat) {
		return true
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			if p.Target.Call != nil {
				name := sanitize(p.Target.Call.Func)
				if funFloat[name] {
					return true
				}
			} else if p.Target.Selector != nil {
				name := sanitize(p.Target.Selector.Root)
				if funFloat[name] {
					return true
				}
			}
		}
		if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "float" {
				return true
			}
		}
	}
	return false
}

func IsFloatPrimaryVars(p *parser.Primary, sanitize func(string) string, floatVars, funFloat map[string]bool) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil:
		if p.Lit.Float != nil {
			return true
		}
	case p.Selector != nil:
		name := sanitize(p.Selector.Root)
		if floatVars[name] {
			return true
		}
	case p.Call != nil:
		name := sanitize(p.Call.Func)
		if funFloat[name] {
			return true
		}
	case p.Group != nil:
		return IsFloatExprVars(p.Group, sanitize, floatVars, funFloat)
	case p.List != nil:
		for _, e := range p.List.Elems {
			if IsFloatExprVars(e, sanitize, floatVars, funFloat) {
				return true
			}
		}
	}
	return false
}
