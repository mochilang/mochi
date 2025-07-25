//go:build slow

package cscode

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func delegateType(params []string, ret string) string {
	if ret == "" || ret == "void" {
		if len(params) == 0 {
			return "Action"
		}
		return fmt.Sprintf("Action<%s>", strings.Join(params, ", "))
	}
	generics := append(params, ret)
	return fmt.Sprintf("Func<%s>", strings.Join(generics, ", "))
}

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func csTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "int"
	case types.Int64Type:
		return "long"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return fmt.Sprintf("List<%s>", csTypeOf(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("Dictionary<%s, %s>", csTypeOf(tt.Key), csTypeOf(tt.Value))
	case types.StructType:
		name := sanitizeName(tt.Name)
		if name == "" {
			return "dynamic"
		}
		return name
	case types.UnionType:
		name := sanitizeName(tt.Name)
		if name == "" {
			return "dynamic"
		}
		return name
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = csTypeOf(p)
		}
		ret := csTypeOf(tt.Return)
		return delegateType(params, ret)
	case types.VoidType:
		return "void"
	case types.AnyType:
		return "dynamic"
	default:
		return "dynamic"
	}
}

func isAny(t types.Type) bool { _, ok := t.(types.AnyType); return ok }

func isInt(t types.Type) bool     { _, ok := t.(types.IntType); return ok }
func isInt64(t types.Type) bool   { _, ok := t.(types.Int64Type); return ok }
func isFloat(t types.Type) bool   { _, ok := t.(types.FloatType); return ok }
func isNumeric(t types.Type) bool { return isInt(t) || isInt64(t) || isFloat(t) }

func contains(sl []string, s string) bool {
	for _, v := range sl {
		if v == s {
			return true
		}
	}
	return false
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	return types.ResolveTypeRef(t, c.env)
}

// exprAliases returns the set of root identifiers referenced in e.
func exprAliases(e *parser.Expr) map[string]struct{} {
	set := map[string]struct{}{}
	collectAliases(e, set)
	return set
}

func collectAliases(e *parser.Expr, out map[string]struct{}) {
	if e == nil {
		return
	}
	if e.Binary == nil {
		return
	}
	collectUnaryAliases(e.Binary.Left, out)
	for _, op := range e.Binary.Right {
		collectPostfixAliases(op.Right, out)
	}
}

func collectUnaryAliases(u *parser.Unary, out map[string]struct{}) {
	if u == nil {
		return
	}
	collectPostfixAliases(u.Value, out)
}

func collectPostfixAliases(p *parser.PostfixExpr, out map[string]struct{}) {
	if p == nil {
		return
	}
	collectPrimaryAliases(p.Target, out)
	for _, op := range p.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				collectAliases(a, out)
			}
		}
		if op.Index != nil {
			if op.Index.Start != nil {
				collectAliases(op.Index.Start, out)
			}
			if op.Index.End != nil {
				collectAliases(op.Index.End, out)
			}
			if op.Index.Step != nil {
				collectAliases(op.Index.Step, out)
			}
		}
	}
}

func collectPrimaryAliases(p *parser.Primary, out map[string]struct{}) {
	if p == nil {
		return
	}
	if p.Selector != nil {
		out[p.Selector.Root] = struct{}{}
	}
	if p.Group != nil {
		collectAliases(p.Group, out)
	}
}

// splitAnd returns a slice of expressions separated by logical AND operators.
func splitAnd(e *parser.Expr) []*parser.Expr {
	if e == nil || e.Binary == nil {
		return nil
	}
	parts := []*parser.Expr{}
	left := e.Binary.Left
	ops := []*parser.BinaryOp{}
	for _, op := range e.Binary.Right {
		if op.Op == "&&" {
			exp := &parser.Expr{Binary: &parser.BinaryExpr{Left: left, Right: ops}}
			parts = append(parts, exp)
			left = &parser.Unary{Value: op.Right}
			ops = nil
			continue
		}
		ops = append(ops, op)
	}
	parts = append(parts, &parser.Expr{Binary: &parser.BinaryExpr{Left: left, Right: ops}})
	return parts
}

// isFetchExpr reports whether e is a plain fetch expression with no postfix operations.
func isFetchExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil {
		return false
	}
	p := u.Value.Target
	if p == nil {
		return false
	}
	return p.Fetch != nil
}

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

func singular(s string) string {
	if strings.HasSuffix(s, "s") && len(s) > 1 {
		return s[:len(s)-1]
	}
	return s
}

func titleCase(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}

func (c *Compiler) newStructName(base string) string {
	name := pascalCase(base)
	if name == "" {
		name = "Auto"
	}
	if _, ok := c.structs[name]; !ok {
		return name
	}
	for {
		c.structCount++
		candidate := fmt.Sprintf("%s%d", name, c.structCount)
		if _, ok := c.structs[candidate]; !ok {
			return candidate
		}
	}
}

// findStructByFields searches the current environment for a struct type with
// the same set of field names as st. Field order does not matter.
func (c *Compiler) findStructByFields(st types.StructType) (string, bool) {
	if c.env == nil {
		return "", false
	}
	fields := map[string]struct{}{}
	for k := range st.Fields {
		fields[k] = struct{}{}
	}
	for name, def := range c.env.Structs() {
		if len(def.Fields) != len(fields) {
			continue
		}
		match := true
		for k := range fields {
			if _, ok := def.Fields[k]; !ok {
				match = false
				break
			}
		}
		if match {
			return name, true
		}
	}
	return "", false
}

// compileStructLiteral builds a struct literal for ml using the provided type.
func (c *Compiler) compileStructLiteral(ml *parser.MapLiteral, st types.StructType) (string, error) {
	items := make([]string, len(ml.Items))
	for i, it := range ml.Items {
		field := ""
		if n, ok := selectorName(it.Key); ok {
			field = sanitizeName(n)
		} else if s, ok := stringLiteral(it.Key); ok {
			field = sanitizeName(s)
		} else {
			field = sanitizeName(fmt.Sprintf("f%d", i))
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("%s = %s", field, v)
	}
	return fmt.Sprintf("new %s { %s }", sanitizeName(st.Name), strings.Join(items, ", ")), nil
}

func (c *Compiler) compileListWithStruct(ll *parser.ListLiteral, st types.StructType) (string, error) {
	elemType := csTypeOf(st)
	if len(ll.Elems) == 0 {
		return fmt.Sprintf("new List<%s>()", elemType), nil
	}
	elems := make([]string, len(ll.Elems))
	for i, e := range ll.Elems {
		ml := e.Binary.Left.Value.Target.Map
		lit, err := c.compileStructLiteral(ml, st)
		if err != nil {
			return "", err
		}
		elems[i] = lit
	}
	return fmt.Sprintf("new List<%s> { %s }", elemType, strings.Join(elems, ", ")), nil
}

// assignTypeNames attaches generated names to anonymous struct types so that
// compiled code can avoid "dynamic" fields. The returned type mirrors t but
// with any unnamed structs replaced by named versions.
func (c *Compiler) assignTypeNames(t types.Type, hint string) types.Type {
	if c.DictMode {
		return t
	}
	switch tt := t.(type) {
	case types.StructType:
		if tt.Name == "" {
			if name, ok := c.findStructByFields(tt); ok {
				tt.Name = name
			} else {
				tt.Name = c.newStructName(hint)
				c.extraStructs = append(c.extraStructs, tt)
			}
		}
		for k, ft := range tt.Fields {
			tt.Fields[k] = c.assignTypeNames(ft, pascalCase(k))
		}
		return tt
	case types.ListType:
		tt.Elem = c.assignTypeNames(tt.Elem, singular(hint)).(types.Type)
		return tt
	case types.MapType:
		tt.Key = c.assignTypeNames(tt.Key, hint+"Key").(types.Type)
		tt.Value = c.assignTypeNames(tt.Value, hint+"Val").(types.Type)
		return tt
	default:
		return t
	}
}

// registerStructs adds all named struct types within t to the compiler
// environment so later expressions can reference them by name.
func (c *Compiler) registerStructs(t types.Type) {
	if c.DictMode {
		return
	}
	switch tt := t.(type) {
	case types.StructType:
		if tt.Name != "" && c.env != nil {
			if _, ok := c.env.GetStruct(tt.Name); !ok {
				c.env.SetStruct(tt.Name, tt)
			}
		}
		for _, ft := range tt.Fields {
			c.registerStructs(ft)
		}
	case types.ListType:
		c.registerStructs(tt.Elem)
	case types.MapType:
		c.registerStructs(tt.Key)
		c.registerStructs(tt.Value)
	}
}

// inferStructFromList attempts to infer a struct definition from a list literal
// and assigns it a unique name based on name. The struct is not registered in
// the environment; callers should do so if needed.
func (c *Compiler) inferStructFromList(ll *parser.ListLiteral, name string) (types.StructType, bool) {
	st, ok := types.InferStructFromList(ll, c.env)
	if !ok {
		return types.StructType{}, false
	}
	base := pascalCase(singular(name))
	if base == "" {
		base = "Auto"
	}
	if existing, ok := c.env.GetStruct(base); ok {
		if types.StructMatches(existing, st.Fields, st.Order) {
			return existing, true
		}
	}
	idx := 1
	nameCandidate := base
	for {
		if _, ok := c.env.GetStruct(nameCandidate); !ok && !c.structs[nameCandidate] {
			break
		}
		nameCandidate = fmt.Sprintf("%s%d", base, idx)
		idx++
	}
	st.Name = nameCandidate
	return st, true
}

// inferStructFromMapEnv infers a struct from a map literal using the provided
// environment for expression types.
func (c *Compiler) inferStructFromMapEnv(ml *parser.MapLiteral, name string, env *types.Env) (types.StructType, bool) {
	st, ok := types.InferStructFromMapEnv(ml, env)
	if !ok {
		return types.StructType{}, false
	}
	base := pascalCase(singular(name))
	if base == "" {
		base = "AnonStruct"
	}
	if existing, ok := env.GetStruct(base); ok {
		if types.StructMatches(existing, st.Fields, st.Order) {
			return existing, true
		}
	}
	idx := 1
	nameCandidate := base
	for {
		if _, ok := env.GetStruct(nameCandidate); !ok && !c.structs[nameCandidate] {
			break
		}
		nameCandidate = fmt.Sprintf("%s%d", base, idx)
		idx++
	}
	st.Name = nameCandidate
	return st, true
}

func (c *Compiler) inferStructFromMap(ml *parser.MapLiteral, name string) (types.StructType, bool) {
	return c.inferStructFromMapEnv(ml, name, c.env)
}

// inferSimpleMap attempts to infer a typed map from the map literal.
func (c *Compiler) inferSimpleMap(ml *parser.MapLiteral) (types.MapType, bool) {
	return types.InferSimpleMap(ml, c.env)
}

// queryEnv builds an environment containing variables introduced by q.
func (c *Compiler) queryEnv(q *parser.QueryExpr) *types.Env {
	if q == nil {
		return c.env
	}
	srcType := c.inferExprType(q.Source)
	var elem types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elem = lt.Elem
	}
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, elem, true)
	for _, f := range q.Froms {
		ft := c.inferExprType(f.Src)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		child.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := c.inferExprType(j.Src)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		if j.Side != nil && (*j.Side == "left" || *j.Side == "right" || *j.Side == "outer") {
			child.SetVar(j.Var, types.OptionType{Elem: je}, true)
			if *j.Side == "right" || *j.Side == "outer" {
				child.SetVar(q.Var, types.OptionType{Elem: elem}, true)
			}
		} else {
			child.SetVar(j.Var, je, true)
		}
	}
	if q.Group != nil {
		child.SetVar(q.Group.Name, types.GroupType{Elem: elem}, true)
	}
	return child
}
