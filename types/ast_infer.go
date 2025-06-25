package types

import "mochi/ast"

// NodeType returns the static type of the given AST node using env when
// available. It handles only a subset of node kinds sufficient for the
// COBOL backend.
func NodeType(n *ast.Node, env *Env) Type {
	if n == nil {
		return AnyType{}
	}
	switch n.Kind {
	case "int":
		return IntType{}
	case "float":
		return FloatType{}
	case "string":
		return StringType{}
	case "bool":
		return BoolType{}
	case "list":
		var elem Type = AnyType{}
		if len(n.Children) > 0 {
			elem = NodeType(n.Children[0], env)
		}
		return ListType{Elem: elem}
	case "selector":
		if env != nil {
			if t, err := env.GetVar(n.Value.(string)); err == nil {
				return t
			}
		}
		return AnyType{}
	case "call":
		if env != nil {
			if t, err := env.GetVar(n.Value.(string)); err == nil {
				if ft, ok := t.(FuncType); ok {
					return ft.Return
				}
			}
		}
		return AnyType{}
	case "binary":
		lt := NodeType(n.Children[0], env)
		rt := NodeType(n.Children[1], env)
		op := n.Value.(string)
		switch op {
		case "+", "-", "*", "/", "%":
			if isFloat(lt) || isFloat(rt) {
				return FloatType{}
			}
			if isInt(lt) && isInt(rt) {
				return IntType{}
			}
			if op == "+" && isString(lt) && isString(rt) {
				return StringType{}
			}
			if op == "+" && isList(lt) && isList(rt) {
				llt := lt.(ListType)
				rlt := rt.(ListType)
				if equalTypes(llt.Elem, rlt.Elem) {
					return llt
				}
			}
			return AnyType{}
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return BoolType{}
		}
	case "group":
		return NodeType(n.Children[0], env)
	case "unary":
		return NodeType(n.Children[0], env)
	case "index":
		base := NodeType(n.Children[0], env)
		if lt, ok := base.(ListType); ok {
			if len(n.Children) > 1 && (n.Children[1].Kind == "start" || n.Children[1].Kind == "end" || len(n.Children) > 2) {
				return lt
			}
			return lt.Elem
		}
		if isString(base) {
			return StringType{}
		}
	case "if_expr":
		thenT := NodeType(n.Children[1], env)
		if len(n.Children) > 2 {
			elseT := NodeType(n.Children[2], env)
			if equalTypes(thenT, elseT) {
				return thenT
			}
			return AnyType{}
		}
		return thenT
	case "match":
		var t Type
		for _, cs := range n.Children[1:] {
			r := NodeType(cs.Children[1], env)
			if t == nil {
				t = r
				continue
			}
			if !equalTypes(t, r) {
				t = AnyType{}
			}
		}
		if t == nil {
			return AnyType{}
		}
		return t
	}
	return AnyType{}
}

// IsString reports whether n has static type string.
func IsString(n *ast.Node, env *Env) bool {
	_, ok := NodeType(n, env).(StringType)
	return ok
}

// IsFloat reports whether n has static type float.
func IsFloat(n *ast.Node, env *Env) bool {
	_, ok := NodeType(n, env).(FloatType)
	return ok
}

// IsList reports whether n has static list type.
func IsList(n *ast.Node, env *Env) bool {
	_, ok := NodeType(n, env).(ListType)
	return ok
}
