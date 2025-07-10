//go:build slow

package luacode

import (
	"fmt"
	"reflect"
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

var luaReserved = map[string]struct{}{
	"and": {}, "break": {}, "do": {}, "else": {}, "elseif": {}, "end": {}, "false": {},
	"for": {}, "function": {}, "goto": {}, "if": {}, "in": {}, "local": {}, "nil": {},
	"not": {}, "or": {}, "repeat": {}, "return": {}, "then": {}, "true": {}, "until": {},
	"while": {},
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
	if b.Len() == 0 || !((b.String()[0] >= 'A' && b.String()[0] <= 'Z') || (b.String()[0] >= 'a' && b.String()[0] <= 'z') || b.String()[0] == '_') {
		return "_" + b.String()
	}
	res := b.String()
	if _, ok := luaReserved[res]; ok {
		return "_" + res
	}
	return res
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
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
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
func isString(t types.Type) bool { _, ok := t.(types.StringType); return ok }
func isList(t types.Type) bool   { _, ok := t.(types.ListType); return ok }
func isMap(t types.Type) bool    { _, ok := t.(types.MapType); return ok }
func isAny(t types.Type) bool    { _, ok := t.(types.AnyType); return ok }
func isNumber(t types.Type) bool {
	switch t.(type) {
	case types.IntType, types.Int64Type, types.FloatType:
		return true
	default:
		return false
	}
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	return types.ResolveTypeRef(t, c.env)
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.StringType)
	return ok
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	_, ok := c.inferUnaryType(u).(types.StringType)
	return ok
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	_, ok := c.inferPostfixType(p).(types.StringType)
	return ok
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.MapType)
	return ok
}

func (c *Compiler) isMapUnary(u *parser.Unary) bool {
	_, ok := c.inferUnaryType(u).(types.MapType)
	return ok
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	_, ok := c.inferPostfixType(p).(types.MapType)
	return ok
}

func (c *Compiler) isNumberExpr(e *parser.Expr) bool {
	t := c.inferExprType(e)
	return isNumber(t)
}

func (c *Compiler) isNumberUnary(u *parser.Unary) bool {
	t := c.inferUnaryType(u)
	return isNumber(t)
}

func (c *Compiler) isNumberPostfix(p *parser.PostfixExpr) bool {
	t := c.inferPostfixType(p)
	return isNumber(t)
}

// listLiteral returns the ListLiteral contained in e if e is a simple list
// literal without any operators applied.
func listLiteral(e *parser.Expr) (*parser.ListLiteral, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 {
		return nil, false
	}
	if p.Target.List != nil {
		return p.Target.List, true
	}
	return nil, false
}

func simpleStringKey(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
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

func extractFormat(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	p := e.Binary.Left
	if p == nil || len(p.Ops) != 0 || p.Value == nil || p.Value.Target.Map == nil {
		return "", false
	}
	for _, it := range p.Value.Target.Map.Items {
		key, ok := simpleStringKey(it.Key)
		if !ok || key != "format" {
			continue
		}
		if it.Value.Binary == nil || len(it.Value.Binary.Right) != 0 {
			continue
		}
		v := it.Value.Binary.Left.Value
		if v != nil && v.Target.Lit != nil && v.Target.Lit.Str != nil {
			return *v.Target.Lit.Str, true
		}
	}
	return "", false
}

func toLuaLiteral(v any) string {
	switch x := v.(type) {
	case nil:
		return "nil"
	case string:
		return fmt.Sprintf("%q", x)
	case int, int32, int64, float64, float32:
		return fmt.Sprint(x)
	case bool:
		if x {
			return "true"
		}
		return "false"
	case []any:
		parts := make([]string, len(x))
		for i, e := range x {
			parts[i] = toLuaLiteral(e)
		}
		return "{" + strings.Join(parts, ", ") + "}"
	case map[string]any:
		parts := make([]string, 0, len(x))
		for k, v := range x {
			parts = append(parts, fmt.Sprintf("[%q]=%s", k, toLuaLiteral(v)))
		}
		return "{" + strings.Join(parts, ", ") + "}"
	default:
		// try via reflection for slices
		rv := reflect.ValueOf(v)
		if rv.Kind() == reflect.Slice {
			n := rv.Len()
			parts := make([]string, n)
			for i := 0; i < n; i++ {
				parts[i] = toLuaLiteral(rv.Index(i).Interface())
			}
			return "{" + strings.Join(parts, ", ") + "}"
		}
		return fmt.Sprint(x)
	}
}
