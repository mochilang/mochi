//go:build archived

package cppcode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
)

// CppVarLookup resolves variable names to their C++ type string.
type CppVarLookup func(name string) (string, bool)

// CppTypeRef converts a static Type to a C++ type representation.
func CppTypeRef(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "string"
	case types.ListType:
		return "vector<" + CppTypeRef(tt.Elem) + ">"
	case types.MapType:
		return "unordered_map<" + CppTypeRef(tt.Key) + ", " + CppTypeRef(tt.Value) + ">"
	case types.StructType:
		return tt.Name
	case types.AnyType:
		return "any"
	}
	return "auto"
}

// InferCppExprType returns the inferred C++ type string for e.
func InferCppExprType(e *parser.Expr, env *types.Env, lookup CppVarLookup) string {
	if e == nil || e.Binary == nil {
		return ""
	}
	return inferCppBinaryExprType(env, lookup, e.Binary)
}

func inferCppBinaryExprType(env *types.Env, lookup CppVarLookup, b *parser.BinaryExpr) string {
	typ := inferCppUnaryType(env, lookup, b.Left)
	for _, op := range b.Right {
		rtyp := inferCppPostfixType(env, lookup, op.Right)
		switch op.Op {
		case "+":
			if strings.HasPrefix(typ, "vector<") || strings.HasPrefix(rtyp, "vector<") {
				elem := strings.TrimSuffix(strings.TrimPrefix(typ, "vector<"), ">")
				if !strings.HasPrefix(typ, "vector<") {
					elem = strings.TrimSuffix(strings.TrimPrefix(rtyp, "vector<"), ">")
				}
				typ = "vector<" + elem + ">"
			} else if typ == "string" || rtyp == "string" {
				typ = "string"
			} else {
				typ = CppBinaryResultType(typ, op.Op, rtyp)
			}
		default:
			typ = CppBinaryResultType(typ, op.Op, rtyp)
		}
	}
	return typ
}

func inferCppUnaryType(env *types.Env, lookup CppVarLookup, u *parser.Unary) string {
	return inferCppPostfixType(env, lookup, u.Value)
}

func inferCppPostfixType(env *types.Env, lookup CppVarLookup, p *parser.PostfixExpr) string {
	typ := inferCppPrimaryType(env, lookup, p.Target)
	for _, op := range p.Ops {
		if op.Index != nil && op.Index.Colon == nil {
			if strings.HasPrefix(typ, "vector<") {
				typ = strings.TrimSuffix(strings.TrimPrefix(typ, "vector<"), ">")
			} else if typ == "string" {
				typ = "char"
			} else if strings.HasPrefix(typ, "unordered_map<") {
				inside := strings.TrimSuffix(strings.TrimPrefix(typ, "unordered_map<"), ">")
				if parts := strings.SplitN(inside, ",", 2); len(parts) == 2 {
					typ = strings.TrimSpace(parts[1])
				} else {
					typ = "auto"
				}
			}
		} else if op.Field != nil {
			if env != nil {
				if st, ok := env.GetStruct(typ); ok {
					if ft, ok := st.Fields[op.Field.Name]; ok {
						typ = CppTypeRef(ft)
					} else {
						typ = "auto"
					}
				} else if strings.HasPrefix(typ, "unordered_map<") {
					inside := strings.TrimSuffix(strings.TrimPrefix(typ, "unordered_map<"), ">")
					if parts := strings.SplitN(inside, ",", 2); len(parts) == 2 {
						typ = strings.TrimSpace(parts[1])
					} else {
						typ = "any"
					}
				}
			} else if strings.HasPrefix(typ, "unordered_map<") {
				inside := strings.TrimSuffix(strings.TrimPrefix(typ, "unordered_map<"), ">")
				if parts := strings.SplitN(inside, ",", 2); len(parts) == 2 {
					typ = strings.TrimSpace(parts[1])
				} else {
					typ = "any"
				}
			}
		}
	}
	return typ
}

func inferCppPrimaryType(env *types.Env, lookup CppVarLookup, p *parser.Primary) string {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return "int"
		}
		if p.Lit.Float != nil {
			return "double"
		}
		if p.Lit.Bool != nil {
			return "bool"
		}
		if p.Lit.Str != nil {
			return "string"
		}
	case p.Struct != nil:
		return p.Struct.Name
	case p.List != nil:
		if len(p.List.Elems) > 0 {
			if t := InferCppExprType(p.List.Elems[0], env, lookup); t != "" {
				return "vector<" + t + ">"
			}
		}
		// default to a vector of any for empty list literals
		return "vector<any>"
	case p.Map != nil:
		keyType := "string"
		valType := ""
		var val string
		for i, it := range p.Map.Items {
			if i == 0 {
				if t := InferCppExprType(it.Key, env, lookup); t != "" {
					keyType = t
				}
				val = InferCppExprType(it.Value, env, lookup)
			} else {
				t := InferCppExprType(it.Value, env, lookup)
				val = mergeCppTypes(val, t)
			}
		}
		if val == "" {
			valType = "any"
		} else {
			valType = val
		}
		if valType == "" {
			valType = "any"
		}
		return "unordered_map<" + keyType + ", " + valType + ">"
	case p.Query != nil:
		rt := InferCppExprType(p.Query.Select, env, lookup)
		if rt == "" || rt == "auto" {
			rt = "any"
		}
		return "vector<" + rt + ">"
	case p.Call != nil:
		switch p.Call.Func {
		case "len", "count", "now":
			return "int"
		case "str", "substr", "input", "json":
			return "string"
		case "sum", "avg":
			return "double"
		case "min":
			if len(p.Call.Args) > 0 {
				t := InferCppExprType(p.Call.Args[0], env, lookup)
				if strings.HasPrefix(t, "vector<") {
					return strings.TrimSuffix(strings.TrimPrefix(t, "vector<"), ">")
				}
			}
			return "auto"
		case "reverse", "concat":
			if len(p.Call.Args) > 0 {
				return InferCppExprType(p.Call.Args[0], env, lookup)
			}
			return "auto"
		case "load":
			return "vector<unordered_map<string,string>>"
		case "fetch":
			return "unordered_map<string,string>"
		case "reduce":
			if len(p.Call.Args) == 3 {
				return InferCppExprType(p.Call.Args[2], env, lookup)
			}
			return "auto"
		case "save":
			return "void"
		}
		return "auto"
	case p.If != nil:
		thenT := InferCppExprType(p.If.Then, env, lookup)
		elseT := ""
		if p.If.ElseIf != nil {
			elseExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{If: p.If.ElseIf}}}}}
			elseT = InferCppExprType(elseExpr, env, lookup)
		} else if p.If.Else != nil {
			elseT = InferCppExprType(p.If.Else, env, lookup)
		}
		if thenT == elseT || elseT == "" {
			return thenT
		}
		if t := mergeCppTypes(thenT, elseT); t != "any" {
			return t
		}
		return "any"
	case p.Selector != nil:
		typ := ""
		if lookup != nil {
			if t, ok := lookup(p.Selector.Root); ok {
				typ = t
			}
		}
		if typ == "" && env != nil {
			if v, err := env.GetVar(p.Selector.Root); err == nil {
				typ = CppTypeRef(v)
			}
		}
		if typ == "" {
			return ""
		}
		if len(p.Selector.Tail) > 0 {
			if env != nil {
				if st, ok := env.GetStruct(typ); ok {
					if ft, ok := st.Fields[p.Selector.Tail[len(p.Selector.Tail)-1]]; ok {
						typ = CppTypeRef(ft)
					} else {
						typ = "any"
					}
				} else if strings.HasPrefix(typ, "unordered_map<") {
					inside := strings.TrimSuffix(strings.TrimPrefix(typ, "unordered_map<"), ">")
					if parts := strings.SplitN(inside, ",", 2); len(parts) == 2 {
						typ = strings.TrimSpace(parts[1])
					} else {
						typ = "any"
					}
				}
			}
		}
		return typ
	}
	return ""
}

// CppBinaryResultType returns the resulting C++ type of applying op between ltyp and rtyp.
func CppBinaryResultType(ltyp, op, rtyp string) string {
	switch op {
	case "+", "-", "*", "/", "%":
		if ltyp == "double" || rtyp == "double" {
			return "double"
		}
		if ltyp == "int" && rtyp == "int" {
			return "int"
		}
		return ltyp
	case "==", "!=", "<", ">", "<=", ">=", "&&", "||", "in":
		return "bool"
	}
	return ltyp
}

func isPrimitive(t string) bool {
	switch t {
	case "int", "double", "bool", "char":
		return true
	}
	return false
}

// mergeCppTypes combines two inferred C++ types into a single type if possible.
// If the types are incompatible the result is "any".
func mergeCppTypes(a, b string) string {
	if a == "any" {
		return b
	}
	if b == "any" {
		return a
	}
	if a == b {
		return a
	}
	if a == "auto" {
		return b
	}
	if b == "auto" {
		return a
	}
	if a == "" {
		return b
	}
	if b == "" {
		return a
	}
	// numeric promotions
	if (a == "int" && b == "double") || (a == "double" && b == "int") {
		return "double"
	}
	if strings.HasPrefix(a, "vector<") && strings.HasPrefix(b, "vector<") {
		ae := strings.TrimSuffix(strings.TrimPrefix(a, "vector<"), ">")
		be := strings.TrimSuffix(strings.TrimPrefix(b, "vector<"), ">")
		elem := mergeCppTypes(ae, be)
		if elem != "any" {
			return "vector<" + elem + ">"
		}
	}
	if strings.HasPrefix(a, "unordered_map<") && strings.HasPrefix(b, "unordered_map<") {
		ainside := strings.TrimSuffix(strings.TrimPrefix(a, "unordered_map<"), ">")
		binside := strings.TrimSuffix(strings.TrimPrefix(b, "unordered_map<"), ">")
		aparts := strings.SplitN(ainside, ",", 2)
		bparts := strings.SplitN(binside, ",", 2)
		if len(aparts) == 2 && len(bparts) == 2 {
			k := mergeCppTypes(strings.TrimSpace(aparts[0]), strings.TrimSpace(bparts[0]))
			v := mergeCppTypes(strings.TrimSpace(aparts[1]), strings.TrimSpace(bparts[1]))
			if k != "any" && v != "any" {
				return "unordered_map<" + k + ", " + v + ">"
			}
		}
	}
	return "any"
}

// mergeTypeSet merges a set of type strings using mergeCppTypes.
func mergeTypeSet(set map[string]struct{}) string {
	out := ""
	for t := range set {
		out = mergeCppTypes(out, t)
	}
	return out
}

func getEmptyListLiteral(e *parser.Expr) *parser.ListLiteral {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	if post.Target.List != nil && len(post.Target.List.Elems) == 0 {
		return post.Target.List
	}
	return nil
}

func isListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	post := u.Value
	return post != nil && post.Target != nil && post.Target.List != nil
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	post := u.Value
	return post != nil && post.Target != nil && post.Target.Lit != nil && post.Target.Lit.Str != nil
}

func getEmptyMapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	if post.Target.Map != nil && len(post.Target.Map.Items) == 0 {
		return post.Target.Map
	}
	return nil
}

func getMapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	return post.Target.Map
}

func inferElemType(e *parser.Expr, env *types.Env, lookup CppVarLookup) string {
	t := InferCppExprType(e, env, lookup)
	if strings.HasPrefix(t, "vector<") {
		return strings.TrimSuffix(strings.TrimPrefix(t, "vector<"), ">")
	}
	if strings.HasPrefix(t, "unordered_map<") {
		inside := strings.TrimSuffix(strings.TrimPrefix(t, "unordered_map<"), ">")
		if parts := strings.SplitN(inside, ",", 2); len(parts) == 2 {
			return strings.TrimSpace(parts[1])
		}
	}
	return ""
}

func getStructLiteral(e *parser.Expr) *parser.StructLiteral {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	return post.Target.Struct
}

func getCallExpr(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	return post.Target.Call
}

func getIntLiteral(e *parser.Expr) *int {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil || post.Target.Lit == nil || post.Target.Lit.Int == nil {
		return nil
	}
	return post.Target.Lit.Int
}

func selectorName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	post := u.Value
	if post == nil || post.Target == nil || post.Target.Selector == nil {
		return "", false
	}
	if len(post.Target.Selector.Tail) != 0 {
		return "", false
	}
	return post.Target.Selector.Root, true
}
