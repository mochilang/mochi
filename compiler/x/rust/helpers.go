package rscode

import (
	"reflect"

	"mochi/parser"
	"mochi/types"
)

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	return p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

// fetchExpr returns the fetch expression if e is a simple fetch expression.
func fetchExpr(e *parser.Expr) (*parser.FetchExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Fetch == nil {
		return nil, false
	}
	return p.Target.Fetch, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
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

// isStringRoot reports whether the base identifier of a postfix expression is a
// string variable in the current environment.
func (c *Compiler) isStringRoot(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	sel := p.Target.Selector
	if sel != nil {
		if c.env != nil {
			if t, err := c.env.GetVar(sel.Root); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

// isStringBase reports whether the target of a postfix expression is a string
// literal or string variable without any indexing applied.
func (c *Compiler) isStringBase(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	sel := p.Target.Selector
	if sel != nil && len(sel.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(sel.Root); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

func equalTypes(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		return true
	}
	if _, ok := b.(types.AnyType); ok {
		return true
	}
	if la, ok := a.(types.ListType); ok {
		if lb, ok := b.(types.ListType); ok {
			return equalTypes(la.Elem, lb.Elem)
		}
	}
	if ma, ok := a.(types.MapType); ok {
		if mb, ok := b.(types.MapType); ok {
			return equalTypes(ma.Key, mb.Key) && equalTypes(ma.Value, mb.Value)
		}
	}
	if ua, ok := a.(types.UnionType); ok {
		if sb, ok := b.(types.StructType); ok {
			if _, ok := ua.Variants[sb.Name]; ok {
				return true
			}
		}
	}
	if ub, ok := b.(types.UnionType); ok {
		if sa, ok := a.(types.StructType); ok {
			if _, ok := ub.Variants[sa.Name]; ok {
				return true
			}
		}
	}
	if isInt64(a) && (isInt64(b) || isInt(b)) {
		return true
	}
	if isInt64(b) && (isInt64(a) || isInt(a)) {
		return true
	}
	if isInt(a) && isInt(b) {
		return true
	}
	return reflect.DeepEqual(a, b)
}

func isInt64(t types.Type) bool {
	_, ok := t.(types.Int64Type)
	return ok
}

func isInt(t types.Type) bool {
	_, ok := t.(types.IntType)
	return ok
}

func isFloat(t types.Type) bool {
	_, ok := t.(types.FloatType)
	return ok
}

func isBool(t types.Type) bool {
	_, ok := t.(types.BoolType)
	return ok
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isList(t types.Type) bool {
	_, ok := t.(types.ListType)
	return ok
}

func isListOfAny(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if _, ok := lt.Elem.(types.AnyType); ok {
			return true
		}
	}
	return false
}

func containsAny(t types.Type) bool {
	switch tt := t.(type) {
	case types.AnyType:
		return true
	case types.ListType:
		return containsAny(tt.Elem)
	case types.MapType:
		return containsAny(tt.Key) || containsAny(tt.Value)
	}
	return false
}

func isMap(t types.Type) bool {
	_, ok := t.(types.MapType)
	return ok
}

func isStruct(t types.Type) bool {
	_, ok := t.(types.StructType)
	return ok
}

func isUnion(t types.Type) bool {
	_, ok := t.(types.UnionType)
	return ok
}

func isAny(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

func contains(ops []string, op string) bool {
	for _, o := range ops {
		if o == op {
			return true
		}
	}
	return false
}

func simpleStringKey(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
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
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

// paramMutated reports whether a parameter with the given name is assigned to within the function body.
func paramMutated(body []*parser.Statement, name string) bool {
	for _, s := range body {
		if stmtMutates(s, name) {
			return true
		}
	}
	return false
}

func stmtMutates(s *parser.Statement, name string) bool {
	switch {
	case s.Assign != nil:
		return s.Assign.Name == name && len(s.Assign.Index) == 0
	case s.For != nil:
		return stmtsMutate(s.For.Body, name)
	case s.While != nil:
		return stmtsMutate(s.While.Body, name)
	case s.If != nil:
		if stmtsMutate(s.If.Then, name) {
			return true
		}
		if s.If.ElseIf != nil {
			if stmtMutates(&parser.Statement{If: s.If.ElseIf}, name) {
				return true
			}
		}
		return stmtsMutate(s.If.Else, name)
	default:
		return false
	}
}

func stmtsMutate(list []*parser.Statement, name string) bool {
	for _, st := range list {
		if stmtMutates(st, name) {
			return true
		}
	}
	return false
}

// rustTypeFrom converts a semantic type to a Rust type string.
func rustTypeFrom(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "i64"
	case types.FloatType:
		return "f64"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return "Vec<" + rustTypeFrom(tt.Elem) + ">"
	case types.MapType:
		return "std::collections::HashMap<" + rustTypeFrom(tt.Key) + ", " + rustTypeFrom(tt.Value) + ">"
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.GroupType:
		return "Group<" + rustTypeFrom(tt.Elem) + ">"
	case types.AnyType:
		return "std::rc::Rc<dyn std::any::Any>"
	default:
		return "std::rc::Rc<dyn std::any::Any>"
	}
}
