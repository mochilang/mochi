//go:build archived

package ftncode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
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
			if types.IsStringExprVars(s.For.Source, sanitizeName, str, nil) {
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

func collectVars(stmts []*parser.Statement, vars map[string]bool, listVars map[string]bool, stringVars map[string]bool, floatVars map[string]bool, listStructs map[string]string, funStr map[string]bool, funFloat map[string]bool, funList map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.Var != nil:
			name := sanitizeName(s.Var.Name)
			vars[name] = true
			if typ, ok := loadTypeName(s.Var.Value); ok {
				listVars[name] = true
				switch typ {
				case "string":
					stringVars[name] = true
				case "float":
					floatVars[name] = true
				default:
					if !isBuiltin(typ) {
						listStructs[name] = typ
					}
				}
			}
			if types.IsListLiteral(s.Var.Value) || types.IsListExprVars(s.Var.Value, sanitizeName, listVars, funList) {
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
					default:
						if !isBuiltin(*s.Var.Type.Generic.Args[0].Simple) {
							listStructs[name] = sanitizeName(*s.Var.Type.Generic.Args[0].Simple)
						}
					}
				}
			}
			if types.IsStringExprVars(s.Var.Value, sanitizeName, stringVars, funStr) {
				stringVars[name] = true
			}
			if types.IsFloatExprVars(s.Var.Value, sanitizeName, floatVars, funFloat) || (s.Var.Type != nil && s.Var.Type.Simple != nil && *s.Var.Type.Simple == "float") {
				floatVars[name] = true
			}
		case s.Let != nil:
			name := sanitizeName(s.Let.Name)
			vars[name] = true
			if typ, ok := loadTypeName(s.Let.Value); ok {
				listVars[name] = true
				switch typ {
				case "string":
					stringVars[name] = true
				case "float":
					floatVars[name] = true
				default:
					if !isBuiltin(typ) {
						listStructs[name] = typ
					}
				}
			}
			if types.IsListLiteral(s.Let.Value) || types.IsListExprVars(s.Let.Value, sanitizeName, listVars, funList) {
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
					default:
						if !isBuiltin(*s.Let.Type.Generic.Args[0].Simple) {
							listStructs[name] = sanitizeName(*s.Let.Type.Generic.Args[0].Simple)
						}
					}
				}
			}
			if types.IsStringExprVars(s.Let.Value, sanitizeName, stringVars, funStr) {
				stringVars[name] = true
			}
			if types.IsFloatExprVars(s.Let.Value, sanitizeName, floatVars, funFloat) || (s.Let.Type != nil && s.Let.Type.Simple != nil && *s.Let.Type.Simple == "float") {
				floatVars[name] = true
			}
		case s.For != nil:
			collectVars(s.For.Body, vars, listVars, stringVars, floatVars, listStructs, funStr, funFloat, funList)
		case s.While != nil:
			collectVars(s.While.Body, vars, listVars, stringVars, floatVars, listStructs, funStr, funFloat, funList)
		case s.If != nil:
			collectVars(s.If.Then, vars, listVars, stringVars, floatVars, listStructs, funStr, funFloat, funList)
			cur := s.If
			for cur.ElseIf != nil {
				cur = cur.ElseIf
				collectVars(cur.Then, vars, listVars, stringVars, floatVars, listStructs, funStr, funFloat, funList)
			}
			if len(cur.Else) > 0 {
				collectVars(cur.Else, vars, listVars, stringVars, floatVars, listStructs, funStr, funFloat, funList)
			}
		}
	}
}
