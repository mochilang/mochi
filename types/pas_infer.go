package types

import (
	"mochi/parser"
	"strings"
)

// PasExprType infers the Mochi type of expression e using env and optional
// variable type hints.
func PasExprType(e *parser.Expr, env *Env, vars map[string]string) Type {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return IntType{}
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 {
		return IntType{}
	}
	p := u.Value
	if p == nil || len(p.Ops) > 0 {
		return IntType{}
	}
	return PasPrimaryType(p.Target, env, vars)
}

// PasPrimaryType infers the Mochi type of primary expression p using env and
// optional variable type hints.
func PasPrimaryType(p *parser.Primary, env *Env, vars map[string]string) Type {
	switch {
	case p == nil:
		return IntType{}
	case p.Lit != nil:
		switch {
		case p.Lit.Str != nil:
			return StringType{}
		case p.Lit.Bool != nil:
			return BoolType{}
		case p.Lit.Float != nil:
			return FloatType{}
		default:
			return IntType{}
		}
	case p.Selector != nil:
		if env != nil {
			if t, err := env.GetVar(p.Selector.Root); err == nil {
				return t
			}
		}
		if vars != nil {
			if v, ok := vars[p.Selector.Root]; ok {
				return parsePasType(v)
			}
		}
	case p.Struct != nil:
		return StructType{Name: p.Struct.Name}
	case p.List != nil:
		var elem Type = IntType{}
		if len(p.List.Elems) > 0 {
			elem = PasExprType(p.List.Elems[0], env, vars)
		}
		return ListType{Elem: elem}
	case p.FunExpr != nil:
		params := make([]Type, len(p.FunExpr.Params))
		for i, pa := range p.FunExpr.Params {
			params[i] = ResolveTypeRef(pa.Type, env)
		}
		ret := ResolveTypeRef(p.FunExpr.Return, env)
		return FuncType{Params: params, Return: ret}
	}
	return IntType{}
}

// parsePasType converts a Pascal type string used by the compiler into a Type.
func parsePasType(s string) Type {
	switch s {
	case "integer":
		return IntType{}
	case "double":
		return FloatType{}
	case "string":
		return StringType{}
	case "boolean":
		return BoolType{}
	case "char":
		return StringType{}
	}
	if strings.HasPrefix(s, "specialize TArray<") && strings.HasSuffix(s, ">") {
		inner := strings.TrimSuffix(strings.TrimPrefix(s, "specialize TArray<"), ">")
		return ListType{Elem: parsePasType(strings.TrimSpace(inner))}
	}
	if strings.HasPrefix(s, "specialize TFPGMap<") && strings.HasSuffix(s, ">") {
		inner := strings.TrimSuffix(strings.TrimPrefix(s, "specialize TFPGMap<"), ">")
		parts := strings.SplitN(inner, ",", 2)
		if len(parts) == 2 {
			return MapType{Key: parsePasType(strings.TrimSpace(parts[0])), Value: parsePasType(strings.TrimSpace(parts[1]))}
		}
	}
	if strings.HasPrefix(s, "function(") || strings.HasPrefix(s, "procedure(") {
		proc := strings.HasPrefix(s, "procedure(")
		endIdx := strings.Index(s, ")")
		if endIdx > 0 {
			paramsPart := s[strings.Index(s, "(")+1 : endIdx]
			rest := strings.TrimSpace(s[endIdx+1:])
			var ret Type = VoidType{}
			if !proc {
				if strings.HasPrefix(rest, ":") {
					ret = parsePasType(strings.TrimSpace(rest[1:]))
				}
			}
			parts := []Type{}
			if strings.TrimSpace(paramsPart) != "" {
				for _, p := range strings.Split(paramsPart, ";") {
					parts = append(parts, parsePasType(strings.TrimSpace(p)))
				}
			}
			return FuncType{Params: parts, Return: ret}
		}
	}
	if s != "" {
		return StructType{Name: s}
	}
	return IntType{}
}
