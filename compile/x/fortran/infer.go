package ftncode

import (
	"strings"

	"mochi/parser"
)

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z')) {
		s = "v_" + s
	}
	return s
}

func collectLoopVars(stmts []*parser.Statement, vars, str map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.For != nil:
			name := sanitizeName(s.For.Name)
			vars[name] = true
			if s.For.RangeEnd == nil {
				vars["i_"+name] = true
			}
			if isStringExpr(s.For.Source, str, nil) {
				str[name] = true
			}
			collectLoopVars(s.For.Body, vars, str)
		case s.If != nil:
			collectLoopVars(s.If.Then, vars, str)
			cur := s.If
			for cur.ElseIf != nil {
				cur = cur.ElseIf
				collectLoopVars(cur.Then, vars, str)
			}
			if len(cur.Else) > 0 {
				collectLoopVars(cur.Else, vars, str)
			}
		case s.While != nil:
			collectLoopVars(s.While.Body, vars, str)
		}
	}
}

func collectVars(stmts []*parser.Statement, vars map[string]bool, listVars map[string]bool, stringVars map[string]bool, floatVars map[string]bool, funStr map[string]bool, funFloat map[string]bool, funList map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.Var != nil:
			name := sanitizeName(s.Var.Name)
			vars[name] = true
			if isListLiteral(s.Var.Value) || isListExpr(s.Var.Value, listVars, funList) {
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
			if isStringExpr(s.Var.Value, stringVars, funStr) {
				stringVars[name] = true
			}
			if isFloatExpr(s.Var.Value, floatVars, funFloat) || (s.Var.Type != nil && s.Var.Type.Simple != nil && *s.Var.Type.Simple == "float") {
				floatVars[name] = true
			}
		case s.Let != nil:
			name := sanitizeName(s.Let.Name)
			vars[name] = true
			if isListLiteral(s.Let.Value) || isListExpr(s.Let.Value, listVars, funList) {
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
			if isStringExpr(s.Let.Value, stringVars, funStr) {
				stringVars[name] = true
			}
			if isFloatExpr(s.Let.Value, floatVars, funFloat) || (s.Let.Type != nil && s.Let.Type.Simple != nil && *s.Let.Type.Simple == "float") {
				floatVars[name] = true
			}
		case s.For != nil:
			collectVars(s.For.Body, vars, listVars, stringVars, floatVars, funStr, funFloat, funList)
		case s.While != nil:
			collectVars(s.While.Body, vars, listVars, stringVars, floatVars, funStr, funFloat, funList)
		case s.If != nil:
			collectVars(s.If.Then, vars, listVars, stringVars, floatVars, funStr, funFloat, funList)
			cur := s.If
			for cur.ElseIf != nil {
				cur = cur.ElseIf
				collectVars(cur.Then, vars, listVars, stringVars, floatVars, funStr, funFloat, funList)
			}
			if len(cur.Else) > 0 {
				collectVars(cur.Else, vars, listVars, stringVars, floatVars, funStr, funFloat, funList)
			}
		}
	}
}

func isListLiteral(e *parser.Expr) bool {
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

func isEmptyListLiteral(e *parser.Expr) bool {
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

func isListExpr(e *parser.Expr, listVars map[string]bool, funList map[string]bool) bool {
	if e == nil {
		return false
	}
	if isListLiteral(e) || isEmptyListLiteral(e) {
		return true
	}
	if len(e.Binary.Right) == 0 && e.Binary.Left != nil {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 0 {
			if u.Value.Target != nil {
				if u.Value.Target.Selector != nil {
					name := sanitizeName(u.Value.Target.Selector.Root)
					if listVars[name] {
						return true
					}
				}
				if u.Value.Target.Call != nil {
					name := sanitizeName(u.Value.Target.Call.Func)
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

func isStringExpr(e *parser.Expr, stringVars map[string]bool, funStr map[string]bool) bool {
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
						name := sanitizeName(v.Target.Selector.Root)
						if stringVars[name] {
							return true
						}
					}
					if v.Target.Call != nil {
						name := sanitizeName(v.Target.Call.Func)
						if funStr[name] {
							return true
						}
					}
					if v.Target.If != nil {
						if isStringIfExpr(v.Target.If, stringVars, funStr) {
							return true
						}
					}
				}
				if v.Target != nil && v.Target.Group != nil {
					return isStringExpr(v.Target.Group, stringVars, funStr)
				}
			}
		}
	}
	return false
}

func isStringIfExpr(ie *parser.IfExpr, stringVars map[string]bool, funStr map[string]bool) bool {
	if ie == nil {
		return false
	}
	if !isStringExpr(ie.Then, stringVars, funStr) {
		return false
	}
	if ie.ElseIf != nil {
		return isStringIfExpr(ie.ElseIf, stringVars, funStr)
	}
	if ie.Else != nil {
		return isStringExpr(ie.Else, stringVars, funStr)
	}
	return false
}

func isFloatExpr(e *parser.Expr, floatVars map[string]bool, funFloat map[string]bool) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if isFloatUnary(e.Binary.Left, floatVars, funFloat) {
		return true
	}
	for _, part := range e.Binary.Right {
		if isFloatPostfix(part.Right, floatVars, funFloat) {
			return true
		}
	}
	return false
}

func isFloatUnary(u *parser.Unary, floatVars map[string]bool, funFloat map[string]bool) bool {
	if u == nil {
		return false
	}
	if isFloatPostfix(u.Value, floatVars, funFloat) {
		return true
	}
	return false
}

func isFloatPostfix(p *parser.PostfixExpr, floatVars map[string]bool, funFloat map[string]bool) bool {
	if p == nil {
		return false
	}
	if isFloatPrimary(p.Target, floatVars, funFloat) {
		return true
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			if p.Target.Call != nil {
				name := sanitizeName(p.Target.Call.Func)
				if funFloat[name] {
					return true
				}
			} else if p.Target.Selector != nil {
				name := sanitizeName(p.Target.Selector.Root)
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

func isFloatPrimary(p *parser.Primary, floatVars map[string]bool, funFloat map[string]bool) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil:
		if p.Lit.Float != nil {
			return true
		}
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if floatVars[name] {
			return true
		}
	case p.Call != nil:
		name := sanitizeName(p.Call.Func)
		if funFloat[name] {
			return true
		}
	case p.Group != nil:
		return isFloatExpr(p.Group, floatVars, funFloat)
	case p.List != nil:
		for _, e := range p.List.Elems {
			if isFloatExpr(e, floatVars, funFloat) {
				return true
			}
		}
	}
	return false
}
