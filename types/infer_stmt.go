package types

import "mochi/parser"

func normalizeName(name string, fn func(string) string) string {
	if fn != nil {
		return fn(name)
	}
	return name
}

// CollectLoopVars scans stmts and records loop variables in vars and string loop variables in str.
func CollectLoopVars(stmts []*parser.Statement, vars, str map[string]bool, normalize func(string) string) {
	for _, s := range stmts {
		switch {
		case s.For != nil:
			name := normalizeName(s.For.Name, normalize)
			vars[name] = true
			if s.For.RangeEnd == nil {
				vars["i_"+name] = true
			}
			if IsStringExpr(s.For.Source, str, nil, normalize) {
				str[name] = true
			}
			CollectLoopVars(s.For.Body, vars, str, normalize)
		case s.If != nil:
			CollectLoopVars(s.If.Then, vars, str, normalize)
			cur := s.If
			for cur.ElseIf != nil {
				cur = cur.ElseIf
				CollectLoopVars(cur.Then, vars, str, normalize)
			}
			if len(cur.Else) > 0 {
				CollectLoopVars(cur.Else, vars, str, normalize)
			}
		case s.While != nil:
			CollectLoopVars(s.While.Body, vars, str, normalize)
		}
	}
}

// CollectVars walks stmts and records declared variables and their inferred types.
func CollectVars(stmts []*parser.Statement, vars map[string]bool, listVars map[string]bool, stringVars map[string]bool, floatVars map[string]bool, funStr map[string]bool, funFloat map[string]bool, funList map[string]bool, normalize func(string) string) {
	for _, s := range stmts {
		switch {
		case s.Var != nil:
			name := normalizeName(s.Var.Name, normalize)
			vars[name] = true
			if IsListLiteral(s.Var.Value) || IsListExpr(s.Var.Value, listVars, funList, normalize) {
				listVars[name] = true
			}
			if s.Var.Type != nil && s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "list" {
				listVars[name] = true
				if len(s.Var.Type.Generic.Args) == 1 && s.Var.Type.Generic.Args[0].Simple != nil {
					switch *s.Var.Type.Generic.Args[0].Simple {
					case "string":
						stringVars[name] = true
					case "float":
						floatVars[name] = true
					}
				}
			}
			if IsStringExpr(s.Var.Value, stringVars, funStr, normalize) {
				stringVars[name] = true
			}
			if IsFloatExpr(s.Var.Value, floatVars, funFloat, normalize) || (s.Var.Type != nil && s.Var.Type.Simple != nil && *s.Var.Type.Simple == "float") {
				floatVars[name] = true
			}
		case s.Let != nil:
			name := normalizeName(s.Let.Name, normalize)
			vars[name] = true
			if IsListLiteral(s.Let.Value) || IsListExpr(s.Let.Value, listVars, funList, normalize) {
				listVars[name] = true
			}
			if s.Let.Type != nil && s.Let.Type.Generic != nil && s.Let.Type.Generic.Name == "list" {
				listVars[name] = true
				if len(s.Let.Type.Generic.Args) == 1 && s.Let.Type.Generic.Args[0].Simple != nil {
					switch *s.Let.Type.Generic.Args[0].Simple {
					case "string":
						stringVars[name] = true
					case "float":
						floatVars[name] = true
					}
				}
			}
			if IsStringExpr(s.Let.Value, stringVars, funStr, normalize) {
				stringVars[name] = true
			}
			if IsFloatExpr(s.Let.Value, floatVars, funFloat, normalize) || (s.Let.Type != nil && s.Let.Type.Simple != nil && *s.Let.Type.Simple == "float") {
				floatVars[name] = true
			}
		case s.For != nil:
			CollectVars(s.For.Body, vars, listVars, stringVars, floatVars, funStr, funFloat, funList, normalize)
		case s.While != nil:
			CollectVars(s.While.Body, vars, listVars, stringVars, floatVars, funStr, funFloat, funList, normalize)
		case s.If != nil:
			CollectVars(s.If.Then, vars, listVars, stringVars, floatVars, funStr, funFloat, funList, normalize)
			cur := s.If
			for cur.ElseIf != nil {
				cur = cur.ElseIf
				CollectVars(cur.Then, vars, listVars, stringVars, floatVars, funStr, funFloat, funList, normalize)
			}
			if len(cur.Else) > 0 {
				CollectVars(cur.Else, vars, listVars, stringVars, floatVars, funStr, funFloat, funList, normalize)
			}
		}
	}
}

// IsListLiteral reports whether e is a list literal.
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

// IsListExpr determines if e evaluates to a list.
func IsListExpr(e *parser.Expr, listVars map[string]bool, funList map[string]bool, normalize func(string) string) bool {
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
					name := normalizeName(u.Value.Target.Selector.Root, normalize)
					if listVars[name] {
						return true
					}
				}
				if u.Value.Target.Call != nil {
					name := normalizeName(u.Value.Target.Call.Func, normalize)
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

// IsStringExpr reports whether e evaluates to a string.
func IsStringExpr(e *parser.Expr, stringVars map[string]bool, funStr map[string]bool, normalize func(string) string) bool {
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
					if v.Target.Selector != nil {
						name := normalizeName(v.Target.Selector.Root, normalize)
						if stringVars[name] {
							return true
						}
					}
					if v.Target.Call != nil {
						name := normalizeName(v.Target.Call.Func, normalize)
						if funStr[name] {
							return true
						}
					}
					if v.Target.If != nil {
						if IsStringIfExpr(v.Target.If, stringVars, funStr, normalize) {
							return true
						}
					}
				}
				if v.Target != nil && v.Target.Group != nil {
					return IsStringExpr(v.Target.Group, stringVars, funStr, normalize)
				}
			}
		}
	}
	return false
}

// IsStringIfExpr reports whether the if-expression yields a string in all branches.
func IsStringIfExpr(ie *parser.IfExpr, stringVars map[string]bool, funStr map[string]bool, normalize func(string) string) bool {
	if ie == nil {
		return false
	}
	if !IsStringExpr(ie.Then, stringVars, funStr, normalize) {
		return false
	}
	if ie.ElseIf != nil {
		return IsStringIfExpr(ie.ElseIf, stringVars, funStr, normalize)
	}
	if ie.Else != nil {
		return IsStringExpr(ie.Else, stringVars, funStr, normalize)
	}
	return false
}

// IsFloatExpr reports whether e evaluates to a float.
func IsFloatExpr(e *parser.Expr, floatVars map[string]bool, funFloat map[string]bool, normalize func(string) string) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if isFloatUnary(e.Binary.Left, floatVars, funFloat, normalize) {
		return true
	}
	for _, part := range e.Binary.Right {
		if isFloatPostfix(part.Right, floatVars, funFloat, normalize) {
			return true
		}
	}
	return false
}

func isFloatUnary(u *parser.Unary, floatVars map[string]bool, funFloat map[string]bool, normalize func(string) string) bool {
	if u == nil {
		return false
	}
	if isFloatPostfix(u.Value, floatVars, funFloat, normalize) {
		return true
	}
	return false
}

func isFloatPostfix(p *parser.PostfixExpr, floatVars map[string]bool, funFloat map[string]bool, normalize func(string) string) bool {
	if p == nil {
		return false
	}
	if isFloatPrimary(p.Target, floatVars, funFloat, normalize) {
		return true
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			if p.Target.Call != nil {
				name := normalizeName(p.Target.Call.Func, normalize)
				if funFloat[name] {
					return true
				}
			} else if p.Target.Selector != nil {
				name := normalizeName(p.Target.Selector.Root, normalize)
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

func isFloatPrimary(p *parser.Primary, floatVars map[string]bool, funFloat map[string]bool, normalize func(string) string) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil:
		if p.Lit.Float != nil {
			return true
		}
	case p.Selector != nil:
		name := normalizeName(p.Selector.Root, normalize)
		if floatVars[name] {
			return true
		}
	case p.Call != nil:
		name := normalizeName(p.Call.Func, normalize)
		if funFloat[name] {
			return true
		}
	case p.Group != nil:
		return IsFloatExpr(p.Group, floatVars, funFloat, normalize)
	case p.List != nil:
		for _, e := range p.List.Elems {
			if IsFloatExpr(e, floatVars, funFloat, normalize) {
				return true
			}
		}
	}
	return false
}
