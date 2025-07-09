//go:build slow

package gocode

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

// joinItems formats a list of items either on a single line or
// across multiple indented lines depending on the length. The
// `indent` value is the current indentation depth and `threshold`
// controls the maximum number of items allowed on one line.
func joinItems(items []string, indent, threshold int) string {
	if len(items) == 0 {
		return ""
	}
	if len(items) <= threshold {
		return strings.Join(items, ", ")
	}
	inner := strings.Join(items, ",\n") + ","
	return "\n" + indentBlock(inner, indent+1) + strings.Repeat("\t", indent)
}

var goReserved = map[string]bool{
	"break": true, "default": true, "func": true, "interface": true, "select": true,
	"case": true, "defer": true, "go": true, "map": true, "struct": true,
	"chan": true, "else": true, "goto": true, "package": true, "switch": true,
	"const": true, "fallthrough": true, "if": true, "range": true, "type": true,
	"continue": true, "for": true, "import": true, "return": true, "var": true,
}

func sanitizeName(name string) string {
	if name == "_" {
		return "v"
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else if r > 127 {
			b.WriteRune('_')
		} else {
			b.WriteRune('_')
		}
	}
	sanitized := b.String()
	if sanitized == "" || !((sanitized[0] >= 'A' && sanitized[0] <= 'Z') || (sanitized[0] >= 'a' && sanitized[0] <= 'z') || sanitized[0] == '_') {
		sanitized = "_" + sanitized
	}
	if goReserved[sanitized] {
		sanitized = "_" + sanitized
	}
	return sanitized
}

func exportName(name string) string {
	if name == "" {
		return ""
	}
	runes := []rune(name)
	if runes[0] >= 'a' && runes[0] <= 'z' {
		runes[0] = runes[0] - 'a' + 'A'
	}
	return string(runes)
}

func goType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType:
		return "int"
	case types.Int64Type:
		return "int64"
	case types.FloatType:
		return "float64"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return "[]" + goType(tt.Elem)
	case types.MapType:
		return fmt.Sprintf("map[%s]%s", goType(tt.Key), goType(tt.Value))
	case types.GroupType:
		return "*data.Group"
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = goType(p)
		}
		ret := goType(tt.Return)
		if ret == "" || ret == "void" {
			return fmt.Sprintf("func(%s)", strings.Join(params, ", "))
		}
		return fmt.Sprintf("func(%s) %s", strings.Join(params, ", "), ret)
	case types.VoidType:
		return ""
	case types.AnyType:
		return "any"
	default:
		return "any"
	}
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

func isNumeric(t types.Type) bool {
	return isInt(t) || isInt64(t) || isFloat(t)
}

func isBool(t types.Type) bool {
	_, ok := t.(types.BoolType)
	return ok
}

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isComparableSimple(t types.Type) bool {
	return isInt(t) || isInt64(t) || isFloat(t) || isBool(t) || isString(t)
}

func isList(t types.Type) bool {
	_, ok := t.(types.ListType)
	return ok
}

func isListOfAny(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if _, ok := lt.Elem.(types.AnyType); ok {
			return true
		}
	}
	return false
}

func containsAny(t types.Type) bool {
	switch tt := t.(type) {
	case types.AnyType:
		return true
	case types.ListType:
		return containsAny(tt.Elem)
	case types.MapType:
		return containsAny(tt.Key) || containsAny(tt.Value)
	}
	return false
}

func isMap(t types.Type) bool {
	_, ok := t.(types.MapType)
	return ok
}

func isStruct(t types.Type) bool {
	_, ok := t.(types.StructType)
	return ok
}

func isUnion(t types.Type) bool {
	_, ok := t.(types.UnionType)
	return ok
}

func isAny(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

func contains(ops []string, op string) bool {
	for _, o := range ops {
		if o == op {
			return true
		}
	}
	return false
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	return types.ResolveTypeRef(t, c.env)
}

func (c *Compiler) castExpr(expr string, from, to types.Type) string {
	fromGo := goType(from)
	toGo := goType(to)
	if toGo == "" || toGo == fromGo || equalTypes(from, to) {
		return expr
	}
	if isNumeric(from) && isNumeric(to) {
		return fmt.Sprintf("%s(%s)", toGo, expr)
	}
	c.use("_cast")
	c.imports["encoding/json"] = true
	return fmt.Sprintf("_cast[%s](%s)", toGo, expr)
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

func simpleStringKey(e *parser.Expr) (string, bool) {
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
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

func (c *Compiler) newVar() string {
	name := fmt.Sprintf("_tmp%d", c.tempVarCount)
	c.tempVarCount++
	return name
}

func collectIdents(e *parser.Expr, out map[string]struct{}) {
	if e == nil || e.Binary == nil {
		return
	}
	var scanPrimary func(*parser.Primary)
	var scanPostfix func(*parser.PostfixExpr)
	var scanUnary func(*parser.Unary)

	scanUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		scanPostfix(u.Value)
	}
	scanPostfix = func(p *parser.PostfixExpr) {
		if p == nil {
			return
		}
		scanPrimary(p.Target)
		for _, op := range p.Ops {
			if op.Index != nil {
				collectIdents(op.Index.Start, out)
				collectIdents(op.Index.End, out)
			} else if op.Call != nil {
				for _, a := range op.Call.Args {
					collectIdents(a, out)
				}
			}
		}
	}
	scanPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		switch {
		case p.Query != nil:
			collectIdents(p.Query.Source, out)
			for _, f := range p.Query.Froms {
				collectIdents(f.Src, out)
			}
			for _, j := range p.Query.Joins {
				collectIdents(j.Src, out)
				collectIdents(j.On, out)
			}
			collectIdents(p.Query.Where, out)
			if p.Query.Group != nil {
				collectIdents(p.Query.Group.Exprs[0], out)
			}
			collectIdents(p.Query.Sort, out)
			collectIdents(p.Query.Skip, out)
			collectIdents(p.Query.Take, out)
			collectIdents(p.Query.Select, out)
		case p.FunExpr != nil:
			collectIdents(p.FunExpr.ExprBody, out)
		case p.If != nil:
			collectIdents(p.If.Cond, out)
			collectIdents(p.If.Then, out)
			collectIdents(p.If.Else, out)
		case p.Match != nil:
			collectIdents(p.Match.Target, out)
			for _, c := range p.Match.Cases {
				collectIdents(c.Pattern, out)
				collectIdents(c.Result, out)
			}
		case p.List != nil:
			for _, el := range p.List.Elems {
				collectIdents(el, out)
			}
		case p.Map != nil:
			for _, it := range p.Map.Items {
				collectIdents(it.Key, out)
				collectIdents(it.Value, out)
			}
		case p.Call != nil:
			for _, a := range p.Call.Args {
				collectIdents(a, out)
			}
		case p.Selector != nil:
			out[p.Selector.Root] = struct{}{}
		case p.Group != nil:
			collectIdents(p.Group, out)
		case p.Generate != nil:
			for _, f := range p.Generate.Fields {
				collectIdents(f.Value, out)
			}
		case p.Fetch != nil:
			collectIdents(p.Fetch.URL, out)
			collectIdents(p.Fetch.With, out)
		case p.Load != nil:
			collectIdents(p.Load.With, out)
		case p.Save != nil:
			collectIdents(p.Save.Src, out)
			collectIdents(p.Save.With, out)
		}
	}

	scanUnary(e.Binary.Left)
	for _, part := range e.Binary.Right {
		scanPostfix(part.Right)
	}
}
