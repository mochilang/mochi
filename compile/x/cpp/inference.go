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
		return "vector<int>"
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
				if t := InferCppExprType(it.Value, env, lookup); t != val {
					val = "any"
				}
			}
		}
		if val == "" {
			valType = "any"
		} else {
			if val == "any" {
				valType = "any"
			} else {
				valType = val
			}
		}
		if valType == "" {
			valType = "any"
		}
		return "unordered_map<" + keyType + ", " + valType + ">"
	case p.Selector != nil:
		if lookup != nil {
			if t, ok := lookup(p.Selector.Root); ok {
				return t
			}
		}
		if env != nil {
			if typ, err := env.GetVar(p.Selector.Root); err == nil {
				if st, ok := typ.(types.StructType); ok {
					ft := st.Fields[p.Selector.Tail[len(p.Selector.Tail)-1]]
					return CppTypeRef(ft)
				}
				return CppTypeRef(typ)
			}
		}
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
