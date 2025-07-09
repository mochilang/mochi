//go:build slow

package zigcode

import (
	"fmt"
	"reflect"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func zigTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "i32"
	case types.Int64Type:
		return "i64"
	case types.FloatType:
		return "f64"
	case types.StringType:
		return "[]const u8"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return "[]const " + zigTypeOf(tt.Elem)
	case types.MapType:
		if _, ok := tt.Key.(types.StringType); ok {
			return fmt.Sprintf("std.StringHashMap(%s)", zigTypeOf(tt.Value))
		}
		return fmt.Sprintf("std.AutoHashMap(%s, %s)", zigTypeOf(tt.Key), zigTypeOf(tt.Value))
	case types.StructType:
		if tt.Name == "" {
			fields := make([]string, len(tt.Order))
			for i, f := range tt.Order {
				fields[i] = fmt.Sprintf("%s: %s", sanitizeName(f), zigTypeOf(tt.Fields[f]))
			}
			return fmt.Sprintf("struct { %s }", strings.Join(fields, " "))
		}
		return sanitizeName(tt.Name)
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = zigTypeOf(p)
		}
		ret := zigTypeOf(tt.Return)
		return fmt.Sprintf("fn(%s) %s", strings.Join(params, ", "), ret)
	case types.VoidType:
		return "void"
	default:
		return "i32"
	}
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
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
			if c.env != nil {
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
	if e == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Selector == nil {
		return false
	}
	return p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func (c *Compiler) newTmp() string {
	name := fmt.Sprintf("_tmp%d", c.tmpCount)
	c.tmpCount++
	return name
}

// newMapTmp returns a temporary variable name specifically for map literals.
// Using a distinct prefix makes the generated code easier to read.
func (c *Compiler) newMapTmp() string {
	name := fmt.Sprintf("_map%d", c.tmpCount)
	c.tmpCount++
	return name
}

func (c *Compiler) newLabel() string {
	name := fmt.Sprintf("blk%d", c.labelCount)
	c.labelCount++
	return name
}

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat("\t", depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
}

func zeroValue(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "0"
	case types.FloatType:
		return "0.0"
	case types.StringType:
		return "\"\""
	case types.BoolType:
		return "false"
	case types.ListType:
		elem := strings.TrimPrefix(zigTypeOf(tt.Elem), "[]const ")
		return fmt.Sprintf("std.ArrayList(%s).init(std.heap.page_allocator)", elem)
	case types.MapType:
		if _, ok := tt.Key.(types.StringType); ok {
			return fmt.Sprintf("std.StringHashMap(%s).init(std.heap.page_allocator)", zigTypeOf(tt.Value))
		}
		return fmt.Sprintf("std.AutoHashMap(%s, %s).init(std.heap.page_allocator)", zigTypeOf(tt.Key), zigTypeOf(tt.Value))
	case types.StructType:
		return fmt.Sprintf("%s{}", sanitizeName(tt.Name))
	default:
		return "undefined"
	}
}

func simpleStringKey(e *parser.Expr) (string, bool) {
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
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
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

// extractMapLiteral returns the map literal contained in the expression if
// it is a simple literal expression. Returns nil otherwise.
func extractMapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	if u.Value == nil || u.Value.Target == nil || len(u.Value.Ops) != 0 {
		return nil
	}
	return u.Value.Target.Map
}

// mapLiteralStruct returns the Zig struct type and initialization for the map
// literal if all keys are simple strings. The ok result will be false if the
// map cannot be represented as a struct.
// mapLiteralStruct builds a struct representation of the map literal. If name
// is non-empty, the returned initialization will use that name as the struct
// type and typ will contain a declaration of the struct type. When name is
// empty, typ will contain the struct type and init will include the inline
// struct definition.
func (c *Compiler) mapLiteralStruct(m *parser.MapLiteral, name string) (typ, init string, ok bool, err error) {
	keys := make([]string, len(m.Items))
	fields := make([]string, len(m.Items))
	inits := make([]string, len(m.Items))
	for i, it := range m.Items {
		k, ok := simpleStringKey(it.Key)
		if !ok {
			return "", "", false, nil
		}
		key := sanitizeName(k)
		valExpr, err := c.compileExpr(it.Value, false)
		if err != nil {
			return "", "", false, err
		}
		valType := zigTypeOf(c.inferExprType(it.Value))
		keys[i] = key
		fields[i] = fmt.Sprintf("%s: %s,", key, valType)
		inits[i] = fmt.Sprintf(".%s = %s", key, valExpr)
	}
	structDef := fmt.Sprintf("struct { %s }", strings.Join(fields, " "))
	if name != "" {
		typ = fmt.Sprintf("const %s = %s;", name, structDef)
		init = fmt.Sprintf("%s{ %s }", name, strings.Join(inits, ", "))
	} else {
		typ = structDef
		init = fmt.Sprintf("%s{ %s }", structDef, strings.Join(inits, ", "))
	}
	return typ, init, true, nil
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

func isInt64(t types.Type) bool  { _, ok := t.(types.Int64Type); return ok }
func isInt(t types.Type) bool    { _, ok := t.(types.IntType); return ok }
func isFloat(t types.Type) bool  { _, ok := t.(types.FloatType); return ok }
func isBool(t types.Type) bool   { _, ok := t.(types.BoolType); return ok }
func isString(t types.Type) bool { _, ok := t.(types.StringType); return ok }
func isList(t types.Type) bool   { _, ok := t.(types.ListType); return ok }

func canInferType(e *parser.Expr, t types.Type) bool {
	if e == nil {
		return false
	}
	if isEmptyListExpr(e) || isEmptyMapExpr(e) {
		return false
	}
	if _, ok := t.(types.AnyType); ok {
		return false
	}
	return true
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: p}}}}
	return c.inferExprType(e)
}

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
			outMap[sanitizeName(v)] = struct{}{}
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
