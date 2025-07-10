//go:build slow

package ccode

import (
	"fmt"
	"reflect"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func equalTypes(a, b types.Type) bool {
	if _, ok := a.(types.AnyType); ok {
		return true
	}
	if _, ok := b.(types.AnyType); ok {
		return true
	}
	if isInt(a) && isInt(b) {
		return true
	}
	if la, ok := a.(types.ListType); ok {
		if lb, ok := b.(types.ListType); ok {
			return equalTypes(la.Elem, lb.Elem)
		}
	}
	return reflect.DeepEqual(a, b)
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
	return isInt(t) || isFloat(t)
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func cTypeFromType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.BoolType:
		return "int"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "char*"
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.ListType:
		elem := cTypeFromType(tt.Elem)
		if elem == "int" {
			return "list_int"
		}
		if elem == "double" {
			return "list_float"
		}
		if elem == "char*" {
			return "list_string"
		}
		if elem == "list_int" {
			return "list_list_int"
		}
		if elem == "_GroupInt" {
			return "list_group_int"
		}
		if st, ok := tt.Elem.(types.StructType); ok {
			return "list_" + sanitizeName(st.Name)
		}
	case types.MapType:
		if isMapIntBoolType(tt) {
			return "map_int_bool"
		}
		if isMapStringIntType(tt) {
			return "map_string_int"
		}
		if isMapIntStringType(tt) {
			return "map_int_string"
		}
		if isMapStringType(tt) {
			return "map_string"
		}
	case types.GroupType:
		if _, ok := tt.Elem.(types.IntType); ok {
			return "_GroupInt"
		}
	case types.FuncType:
		ret := cTypeFromType(tt.Return)
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = cTypeFromType(p)
		}
		return fmt.Sprintf("%s (*)(%s)", ret, strings.Join(params, ", "))
	}
	return "int"
}

func isListListIntType(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if inner, ok2 := lt.Elem.(types.ListType); ok2 {
			switch inner.Elem.(type) {
			case types.IntType, types.BoolType:
				return true
			}
		}
	}
	return false
}

func isListIntType(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if _, ok := lt.Elem.(types.IntType); ok {
			return true
		}
	}
	return false
}

func isListStringType(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if _, ok2 := lt.Elem.(types.StringType); ok2 {
			return true
		}
	}
	return false
}

func isListFloatType(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if _, ok2 := lt.Elem.(types.FloatType); ok2 {
			return true
		}
	}
	return false
}

func isMapIntBoolType(t types.Type) bool {
	if mt, ok := t.(types.MapType); ok {
		if _, ok := mt.Key.(types.IntType); ok {
			if _, ok2 := mt.Value.(types.BoolType); ok2 {
				return true
			}
		}
	}
	return false
}

func isMapStringType(t types.Type) bool {
	if mt, ok := t.(types.MapType); ok {
		if _, ok := mt.Key.(types.StringType); ok {
			if _, ok2 := mt.Value.(types.StringType); ok2 {
				return true
			}
		}
	}
	return false
}

func isMapStringIntType(t types.Type) bool {
	if mt, ok := t.(types.MapType); ok {
		if _, ok := mt.Key.(types.StringType); ok {
			if _, ok2 := mt.Value.(types.IntType); ok2 {
				return true
			}
		}
	}
	return false
}

func isMapIntStringType(t types.Type) bool {
	if mt, ok := t.(types.MapType); ok {
		if _, ok := mt.Key.(types.IntType); ok {
			if _, ok2 := mt.Value.(types.StringType); ok2 {
				return true
			}
		}
	}
	return false
}

// defaultCValue returns a zero value literal for the given type.
func defaultCValue(t types.Type) string {
	switch t.(type) {
	case types.StringType:
		return "NULL"
	case types.FloatType, types.IntType, types.BoolType:
		return "0"
	default:
		return "0"
	}
}

// listElemType returns the element type of e if it is a list expression with a
// known element type in env.
func listElemType(e *parser.Expr, env *types.Env) types.Type {
	if env == nil || e == nil {
		return nil
	}
	// First try the stricter checker to avoid falling back to `any`.
	if lt, ok := types.CheckExprType(e, env).(types.ListType); ok && !types.ContainsAny(lt.Elem) {
		return lt.Elem
	}
	// If it's a literal list, infer element type from all elements.
	if types.IsListLiteral(e) {
		ll := e.Binary.Left.Value.Target.List
		if len(ll.Elems) > 0 {
			var elem types.Type
			for i, el := range ll.Elems {
				t := types.CheckExprType(el, env)
				if types.ContainsAny(t) {
					elem = nil
					break
				}
				if i == 0 {
					elem = t
				} else if !equalTypes(elem, t) {
					elem = nil
					break
				}
			}
			if elem != nil {
				return elem
			}
		}
	}
	if lt, ok := types.ExprType(e, env).(types.ListType); ok {
		if !types.ContainsAny(lt.Elem) {
			return lt.Elem
		}
	}
	return nil
}

func isListMapStringType(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		return isMapStringType(lt.Elem)
	}
	return false
}

func resolveTypeRef(t *parser.TypeRef, env *types.Env) types.Type {
	if t == nil {
		return types.IntType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p, env)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return, env)
		}
		return types.FuncType{Params: params, Return: ret}
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
		case "void":
			return types.VoidType{}
		default:
			if env != nil {
				if st, ok := env.GetStruct(*t.Simple); ok {
					return st
				}
				if ut, ok := env.GetUnion(*t.Simple); ok {
					return ut
				}
			}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return types.ListType{Elem: resolveTypeRef(t.Generic.Args[0], env)}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return types.MapType{Key: resolveTypeRef(t.Generic.Args[0], env), Value: resolveTypeRef(t.Generic.Args[1], env)}
		}
	}
	return types.AnyType{}
}

func identName(e *parser.Expr) (string, bool) {
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

func formatFuncPtrDecl(typ, name, val string) string {
	if strings.Contains(typ, "(*") {
		typ = strings.Replace(typ, "(*", "(*"+name, 1)
		return fmt.Sprintf("%s = %s;", typ, val)
	}
	return fmt.Sprintf("%s %s = %s;", typ, name, val)
}

// --- Helpers for closures ---

// freeVars returns the free variable names used inside fn.
func freeVars(fn *parser.FunExpr, params []string) []string {
	vars := map[string]struct{}{}
	scanExpr(fn.ExprBody, vars)
	for _, st := range fn.BlockBody {
		scanStmt(st, vars)
	}
	outMap := map[string]struct{}{}
	for v := range vars {
		skip := false
		for _, p := range params {
			if p == sanitizeName(v) {
				skip = true
				break
			}
		}
		if !skip {
			outMap[v] = struct{}{}
		}
	}
	out := make([]string, 0, len(outMap))
	for k := range outMap {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// freeVarsStmt returns the free variable names used inside the function statement.
func freeVarsStmt(fn *parser.FunStmt, params []string) []string {
	vars := map[string]struct{}{}
	for _, st := range fn.Body {
		scanStmt(st, vars)
	}
	outMap := map[string]struct{}{}
	for v := range vars {
		skip := false
		for _, p := range params {
			if p == sanitizeName(v) {
				skip = true
				break
			}
		}
		if !skip {
			outMap[v] = struct{}{}
		}
	}
	out := make([]string, 0, len(outMap))
	for k := range outMap {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

func scanStmt(s *parser.Statement, vars map[string]struct{}) {
	switch {
	case s.Let != nil:
		scanExpr(s.Let.Value, vars)
	case s.Var != nil:
		scanExpr(s.Var.Value, vars)
	case s.Assign != nil:
		scanExpr(s.Assign.Value, vars)
	case s.Return != nil:
		scanExpr(s.Return.Value, vars)
	case s.Expr != nil:
		scanExpr(s.Expr.Expr, vars)
	case s.For != nil:
		scanExpr(s.For.Source, vars)
		scanExpr(s.For.RangeEnd, vars)
		for _, st := range s.For.Body {
			scanStmt(st, vars)
		}
	case s.While != nil:
		scanExpr(s.While.Cond, vars)
		for _, st := range s.While.Body {
			scanStmt(st, vars)
		}
	case s.If != nil:
		scanExpr(s.If.Cond, vars)
		for _, st := range s.If.Then {
			scanStmt(st, vars)
		}
		if s.If.ElseIf != nil {
			scanStmt(&parser.Statement{If: s.If.ElseIf}, vars)
		}
		for _, st := range s.If.Else {
			scanStmt(st, vars)
		}
	case s.Test != nil:
		for _, st := range s.Test.Body {
			scanStmt(st, vars)
		}
	case s.Expect != nil:
		scanExpr(s.Expect.Value, vars)
	}
}

func scanExpr(e *parser.Expr, vars map[string]struct{}) {
	if e == nil {
		return
	}
	scanUnary(e.Binary.Left, vars)
	for _, op := range e.Binary.Right {
		scanPostfix(op.Right, vars)
	}
}

func scanUnary(u *parser.Unary, vars map[string]struct{}) {
	if u == nil {
		return
	}
	scanPostfix(u.Value, vars)
}

func scanPostfix(p *parser.PostfixExpr, vars map[string]struct{}) {
	if p == nil {
		return
	}
	scanPrimary(p.Target, vars)
	for _, op := range p.Ops {
		if op.Index != nil {
			scanExpr(op.Index.Start, vars)
			scanExpr(op.Index.End, vars)
		}
		if op.Call != nil {
			for _, a := range op.Call.Args {
				scanExpr(a, vars)
			}
		}
	}
}

func scanPrimary(p *parser.Primary, vars map[string]struct{}) {
	if p == nil {
		return
	}
	if p.Selector != nil {
		vars[p.Selector.Root] = struct{}{}
	}
	if p.Group != nil {
		scanExpr(p.Group, vars)
	}
	if p.FunExpr != nil {
		scanExpr(p.FunExpr.ExprBody, vars)
		for _, st := range p.FunExpr.BlockBody {
			scanStmt(st, vars)
		}
	}
	if p.List != nil {
		for _, e := range p.List.Elems {
			scanExpr(e, vars)
		}
	}
	if p.Map != nil {
		for _, it := range p.Map.Items {
			if _, ok := types.SimpleStringKey(it.Key); !ok {
				scanExpr(it.Key, vars)
			}
			scanExpr(it.Value, vars)
		}
	}
	if p.Call != nil {
		for _, a := range p.Call.Args {
			scanExpr(a, vars)
		}
	}
}
