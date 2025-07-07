//go:build slow

package cljcode

import (
	"reflect"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z') || ('0' <= r && r <= '9' && i > 0) {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" {
		return "_"
	}
	if !(s[0] >= 'a' && s[0] <= 'z' || s[0] >= 'A' && s[0] <= 'Z' || s[0] == '_') {
		s = "_" + s
	}
	return s
}

func equalTypes(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		return true
	}
	if _, ok := b.(types.AnyType); ok {
		return true
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

func isNumber(t types.Type) bool {
	return isInt(t) || isFloat(t) || isInt64(t)
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isList(t types.Type) bool {
	_, ok := t.(types.ListType)
	return ok
}

func isMap(t types.Type) bool {
	_, ok := t.(types.MapType)
	return ok
}

func isStruct(t types.Type) bool {
	switch t.(type) {
	case types.StructType, types.UnionType:
		return true
	default:
		return false
	}
}

func isAny(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

func contains(sl []string, s string) bool {
	for _, v := range sl {
		if v == s {
			return true
		}
	}
	return false
}

// cljTypeOf returns a human friendly type string for t.
func cljTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "float"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return "list of " + cljTypeOf(tt.Elem)
	case types.MapType:
		return "map of " + cljTypeOf(tt.Key) + " to " + cljTypeOf(tt.Value)
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.FuncType:
		return "function"
	case types.VoidType:
		return "void"
	case types.AnyType:
		return "any"
	default:
		return "any"
	}
}

// varsInExpr returns the set of root variable names referenced in e.
func varsInExpr(e *parser.Expr) map[string]bool {
	vars := map[string]bool{}
	var walkExpr func(*parser.Expr)
	var walkUnary func(*parser.Unary)
	var walkPostfix func(*parser.PostfixExpr)
	var walkPrimary func(*parser.Primary)

	walkExpr = func(e *parser.Expr) {
		if e == nil {
			return
		}
		walkUnary(e.Binary.Left)
		for _, op := range e.Binary.Right {
			walkPostfix(op.Right)
		}
	}

	walkUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		walkPostfix(u.Value)
	}

	walkPostfix = func(pf *parser.PostfixExpr) {
		if pf == nil {
			return
		}
		walkPrimary(pf.Target)
		for _, op := range pf.Ops {
			if op.Call != nil {
				for _, a := range op.Call.Args {
					walkExpr(a)
				}
			}
			if op.Index != nil {
				if op.Index.Start != nil {
					walkExpr(op.Index.Start)
				}
				if op.Index.End != nil {
					walkExpr(op.Index.End)
				}
				if op.Index.Step != nil {
					walkExpr(op.Index.Step)
				}
			}
			if op.Cast != nil {
				// ignore type casts
			}
		}
	}

	walkPrimary = func(pr *parser.Primary) {
		if pr == nil {
			return
		}
		switch {
		case pr.Selector != nil:
			vars[pr.Selector.Root] = true
		case pr.Struct != nil:
			for _, f := range pr.Struct.Fields {
				walkExpr(f.Value)
			}
		case pr.Call != nil:
			for _, a := range pr.Call.Args {
				walkExpr(a)
			}
		case pr.List != nil:
			for _, el := range pr.List.Elems {
				walkExpr(el)
			}
		case pr.Map != nil:
			for _, it := range pr.Map.Items {
				walkExpr(it.Key)
				walkExpr(it.Value)
			}
		case pr.Match != nil:
			walkExpr(pr.Match.Target)
			for _, cs := range pr.Match.Cases {
				walkExpr(cs.Pattern)
				walkExpr(cs.Result)
			}
		case pr.If != nil:
			walkExpr(pr.If.Cond)
			walkExpr(pr.If.Then)
			if pr.If.ElseIf != nil {
				walkExpr(pr.If.ElseIf.Cond)
				walkExpr(pr.If.ElseIf.Then)
				if pr.If.ElseIf.Else != nil {
					walkExpr(pr.If.ElseIf.Else)
				}
			}
			if pr.If.Else != nil {
				walkExpr(pr.If.Else)
			}
		case pr.Generate != nil:
			for _, f := range pr.Generate.Fields {
				walkExpr(f.Value)
			}
		case pr.Fetch != nil:
			walkExpr(pr.Fetch.URL)
			if pr.Fetch.With != nil {
				walkExpr(pr.Fetch.With)
			}
		case pr.Load != nil:
			if pr.Load.With != nil {
				walkExpr(pr.Load.With)
			}
		case pr.Save != nil:
			walkExpr(pr.Save.Src)
			if pr.Save.With != nil {
				walkExpr(pr.Save.With)
			}
		case pr.Query != nil:
			// ignore nested queries
		}
		if pr.Group != nil {
			walkExpr(pr.Group)
		}
	}

	walkExpr(e)
	return vars
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return types.ListType{Elem: c.resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return types.MapType{Key: c.resolveTypeRef(args[0]), Value: c.resolveTypeRef(args[1])}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if c != nil && c.env != nil {
				if st, ok := c.env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := c.env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
			return types.AnyType{}
		}
	}
	return types.AnyType{}
}

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

func isIdentExpr(e *parser.Expr) (string, bool) {
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

// callPattern returns the call expression if e is a direct function call with no operators.
func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
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

// isListPushCall returns the variable name and argument if the expression is a
// simple list.push(x) call.
func isListPushCall(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", nil, false
	}
	p := u.Value
	if p.Target.Selector == nil || len(p.Target.Selector.Tail) != 1 {
		return "", nil, false
	}
	if p.Target.Selector.Tail[0] != "push" {
		return "", nil, false
	}
	if len(p.Ops) != 1 || p.Ops[0].Call == nil || len(p.Ops[0].Call.Args) != 1 {
		return "", nil, false
	}
	return p.Target.Selector.Root, p.Ops[0].Call.Args[0], true
}
