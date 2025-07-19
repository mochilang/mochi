//go:build slow

package pascode

import (
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

var pasReserved = map[string]struct{}{
	"and": {}, "array": {}, "begin": {}, "case": {}, "const": {}, "div": {},
	"do": {}, "downto": {}, "else": {}, "end": {}, "file": {}, "for": {},
	"function": {}, "goto": {}, "if": {}, "in": {}, "label": {}, "mod": {},
	"nil": {}, "not": {}, "of": {}, "or": {}, "packed": {}, "procedure": {},
	"program": {}, "record": {}, "repeat": {}, "set": {}, "then": {}, "to": {},
	"type": {}, "until": {}, "var": {}, "while": {}, "with": {},
	"result": {},
	"length": {},
}

func sanitizeName(name string) string {
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	if b.Len() == 0 {
		return "_"
	}
	res := b.String()
	if _, ok := pasReserved[strings.ToLower(res)]; ok {
		return "_" + res
	}
	return res
}

// pascalCase converts a file or identifier name like "print_hello" to
// "PrintHello" suitable for use as a Pascal program identifier.
func pascalCase(s string) string {
	parts := strings.FieldsFunc(s, func(r rune) bool {
		return r == '_' || r == '-' || r == ' ' || r == '.'
	})
	for i, p := range parts {
		if p == "" {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + strings.ToLower(p[1:])
	}
	return sanitizeName(strings.Join(parts, ""))
}

func rootPrimary(e *parser.Expr) *parser.Primary {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return nil
	}
	return e.Binary.Left.Value.Target
}

func isQueryExpr(e *parser.Expr) bool {
	p := rootPrimary(e)
	return p != nil && p.Query != nil && len(e.Binary.Right) == 0
}

func inferQueryType(q *parser.QueryExpr, env *types.Env) types.Type {
	srcT := types.TypeOfExprBasic(q.Source, env)
	var elem types.Type = types.AnyType{}
	if lt, ok := srcT.(types.ListType); ok {
		elem = lt.Elem
	}
	child := types.NewEnv(env)
	child.SetVar(q.Var, elem, true)
	for _, f := range q.Froms {
		ft := types.TypeOfExprBasic(f.Src, env)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		child.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := types.TypeOfExprBasic(j.Src, env)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		child.SetVar(j.Var, je, true)
	}
	if q.Group != nil {
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.AnyType{}, true)
		elem = types.TypeOfExprBasic(q.Select, genv)
		if asMapLiteral(q.Select) != nil {
			elem = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
		}
		return types.ListType{Elem: elem}
	}
	elem = types.TypeOfExprBasic(q.Select, child)
	if asMapLiteral(q.Select) != nil {
		elem = types.MapType{Key: types.StringType{}, Value: types.AnyType{}}
	}
	return types.ListType{Elem: elem}
}

func (c *Compiler) newTypedVar(typ string) string {
	name := fmt.Sprintf("_tmp%d", c.tempVarCount)
	c.tempVarCount++
	if c.tempVars == nil {
		c.tempVars = make(map[string]string)
	}
	c.tempVars[name] = typ
	if c.varTypes != nil {
		c.varTypes[name] = typ
	}
	if c.env != nil {
		c.env.SetVar(name, parsePasType(typ), true)
	}
	return name
}

func (c *Compiler) newVar() string {
	return c.newTypedVar("integer")
}

// defaultValue returns a zero value literal for the given type.
func defaultValue(t types.Type) string {
	switch t.(type) {
	case types.IntType, types.Int64Type:
		return "0"
	case types.FloatType:
		return "0.0"
	case types.StringType:
		return "\"\""
	case types.BoolType:
		return "False"
	case types.ListType, types.MapType, types.AnyType, types.StructType, types.UnionType, types.FuncType:
		return "nil"
	default:
		return "0"
	}
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func isListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil {
		return false
	}
	return u.Value.Target.List != nil
}

func isMapLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil {
		return false
	}
	return u.Value.Target.Map != nil
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil {
		return false
	}
	return u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil
}

func isBoolLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || u.Value.Target == nil {
		return false
	}
	return u.Value.Target.Lit != nil && u.Value.Target.Lit.Bool != nil
}

func isStringSliceExpr(e *parser.Expr, env *types.Env, vars map[string]string) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || len(u.Value.Ops) != 1 {
		return false
	}
	idx := u.Value.Ops[0].Index
	if idx == nil || (idx.Colon == nil && idx.End == nil) {
		return false
	}
	t := u.Value.Target
	if t == nil {
		return false
	}
	if t.Lit != nil && t.Lit.Str != nil {
		return true
	}
	if t.Selector != nil {
		name := t.Selector.Root
		if env != nil {
			if ty, err := env.GetVar(name); err == nil {
				if _, ok := ty.(types.StringType); ok {
					return true
				}
			}
		}
		if vars != nil {
			if s, ok := vars[name]; ok && s == "string" {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if isListLiteral(e) {
		return true
	}
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	return c.isListUnary(e.Binary.Left)
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if isMapLiteral(e) {
		return true
	}
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	return c.isMapUnary(e.Binary.Left)
}

// isStringExpr reports whether e is a string literal or an identifier of string
// type according to the current environment.
func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	if isStringLiteral(e) {
		return true
	}
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	return c.isStringUnary(e.Binary.Left)
}

func (c *Compiler) isMapUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return c.isMapPostfix(u.Value)
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isMapPrimary(p.Target)
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	switch {
	case p == nil:
		return false
	case p.Map != nil:
		return true
	case p.Selector != nil:
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
		if c.varTypes != nil {
			if vt, ok := c.varTypes[p.Selector.Root]; ok {
				if strings.HasPrefix(vt, "specialize TFPGMap<") {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isListUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return c.isListPostfix(u.Value)
}

func (c *Compiler) isListPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isListPrimary(p.Target)
}

func (c *Compiler) isListPrimary(p *parser.Primary) bool {
	switch {
	case p == nil:
		return false
	case p.List != nil:
		return true
	case p.Selector != nil:
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
		if c.varTypes != nil {
			if vt, ok := c.varTypes[p.Selector.Root]; ok {
				if strings.HasPrefix(vt, "specialize TArray<") {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return c.isStringPostfix(u.Value)
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isStringPrimary(p.Target)
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	switch {
	case p == nil:
		return false
	case p.Lit != nil && p.Lit.Str != nil:
		return true
	case p.Selector != nil:
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
		if c.varTypes != nil {
			if vt, ok := c.varTypes[p.Selector.Root]; ok {
				if vt == "string" {
					return true
				}
			}
		}
	}
	return false
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func parsePasType(s string) types.Type {
	s = strings.TrimSpace(s)
	if strings.HasSuffix(s, " is nested") {
		s = strings.TrimSpace(strings.TrimSuffix(s, " is nested"))
	}
	switch s {
	case "integer":
		return types.IntType{}
	case "double":
		return types.FloatType{}
	case "string":
		return types.StringType{}
	case "boolean":
		return types.BoolType{}
	case "char":
		return types.StringType{}
	case "Variant":
		return types.AnyType{}
	}
	if strings.HasPrefix(s, "specialize TArray<") && strings.HasSuffix(s, ">") {
		inner := strings.TrimSuffix(strings.TrimPrefix(s, "specialize TArray<"), ">")
		return types.ListType{Elem: parsePasType(strings.TrimSpace(inner))}
	}
	if strings.HasPrefix(s, "function(") || strings.HasPrefix(s, "procedure(") {
		proc := strings.HasPrefix(s, "procedure(")
		endIdx := strings.Index(s, ")")
		if endIdx > 0 {
			paramsPart := s[strings.Index(s, "(")+1 : endIdx]
			rest := strings.TrimSpace(s[endIdx+1:])
			var ret types.Type = types.VoidType{}
			if !proc {
				if strings.HasPrefix(rest, ":") {
					ret = parsePasType(strings.TrimSpace(rest[1:]))
				}
			}
			parts := []types.Type{}
			if strings.TrimSpace(paramsPart) != "" {
				for _, p := range strings.Split(paramsPart, ";") {
					parts = append(parts, parsePasType(strings.TrimSpace(p)))
				}
			}
			return types.FuncType{Params: parts, Return: ret}
		}
	}
	if idx := strings.Index(s, ":"); idx > 0 && !strings.ContainsAny(s[:idx], "<(") {
		return parsePasType(strings.TrimSpace(s[idx+1:]))
	}
	if strings.HasPrefix(s, "specialize TFPGMap<") && strings.HasSuffix(s, ">") {
		inner := strings.TrimSuffix(strings.TrimPrefix(s, "specialize TFPGMap<"), ">")
		parts := strings.SplitN(inner, ",", 2)
		if len(parts) == 2 {
			return types.MapType{Key: parsePasType(strings.TrimSpace(parts[0])), Value: parsePasType(strings.TrimSpace(parts[1]))}
		}
	}
	if s != "" {
		return types.StructType{Name: s}
	}
	return types.IntType{}
}

func (c *Compiler) varType(name string) types.Type {
	if c.env != nil {
		if t, err := c.env.GetVar(name); err == nil {
			return t
		}
	}
	if c.varTypes != nil {
		if s, ok := c.varTypes[name]; ok {
			return parsePasType(s)
		}
	}
	return types.IntType{}
}

func (c *Compiler) listElemType(p *parser.Primary) string {
	t := types.TypeOfPrimary(p, c.env)
	if lt, ok := t.(types.ListType); ok {
		return typeString(lt.Elem)
	}
	if p.Selector != nil && c.varTypes != nil {
		if vt, ok := c.varTypes[p.Selector.Root]; ok {
			if strings.HasPrefix(vt, "specialize TArray<") {
				inner := strings.TrimSuffix(strings.TrimPrefix(vt, "specialize TArray<"), ">")
				return inner
			}
		}
	}
	return "integer"
}

func (c *Compiler) elemTypeOfUnary(u *parser.Unary) string {
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return "Variant"
	}
	return c.listElemType(u.Value.Target)
}

func (c *Compiler) elemTypeOfPostfix(p *parser.PostfixExpr) string {
	if p == nil || len(p.Ops) != 0 {
		return "Variant"
	}
	return c.listElemType(p.Target)
}

// elemTypeOfExpr returns the Pascal type string of the element type if e is a list expression.
func (c *Compiler) elemTypeOfExpr(e *parser.Expr) string {
	if t := types.TypeOfExpr(e, c.env); t != nil {
		if lt, ok := t.(types.ListType); ok {
			elem := typeString(lt.Elem)
			if elem != "Variant" {
				return elem
			}
		}
	}
	// try to infer from append-like call
	if e != nil && e.Binary != nil {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil && u.Value.Target != nil && u.Value.Target.Call != nil {
			call := u.Value.Target.Call
			if (call.Func == "append" || call.Func == "push") && len(call.Args) > 0 {
				return c.elemTypeOfExpr(call.Args[0])
			}
		}
	}
	return "Variant"
}

// selectorName returns the identifier name if e is a bare selector
// expression like `foo` with no postfix operations.
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

// stringKey returns the literal or selector name if e is a simple map key.
func stringKey(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 {
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

func asMapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil || e.Binary.Left.Value.Target == nil {
		return nil
	}
	return e.Binary.Left.Value.Target.Map
}
