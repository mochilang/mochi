//go:build slow

package gocode

import (
	"bytes"
	"fmt"
	"reflect"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// ensure reflect is referenced so the import is not optimized away
var _ = reflect.TypeOf(0)

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
	sanitized = strings.TrimLeft(sanitized, "_")
	if sanitized == "" || !((sanitized[0] >= 'A' && sanitized[0] <= 'Z') || (sanitized[0] >= 'a' && sanitized[0] <= 'z')) {
		sanitized = "v" + sanitized
	}
	if goReserved[sanitized] || sanitized == "data" {
		sanitized = sanitized + "Var"
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
	s := string(runes)
	if strings.HasSuffix(s, "Id") {
		s = s[:len(s)-2] + "ID"
	}
	return s
}

func singular(name string) string {
	if len(name) > 1 && strings.HasSuffix(name, "s") {
		return name[:len(name)-1]
	}
	return name
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
	case types.OptionType:
		elem := goType(tt.Elem)
		if elem == "" {
			elem = "any"
		}
		return "*" + elem
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

func equalTypes(a, b types.Type) bool { return types.EqualTypes(a, b) }

func isInt64(t types.Type) bool   { return types.IsInt64Type(t) }
func isInt(t types.Type) bool     { return types.IsIntType(t) }
func isFloat(t types.Type) bool   { return types.IsFloatType(t) }
func isNumeric(t types.Type) bool { return types.IsNumericType(t) }
func isBool(t types.Type) bool    { return types.IsBoolType(t) }
func isString(t types.Type) bool  { return types.IsStringType(t) }

func isComparableSimple(t types.Type) bool {
	return isInt(t) || isInt64(t) || isFloat(t) || isBool(t) || isString(t)
}

func isList(t types.Type) bool { return types.IsListType(t) }

func isListOfAny(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		_, ok := lt.Elem.(types.AnyType)
		return ok
	}
	return false
}

// isTypedSimpleList reports whether t is a list whose elements are of a
// simple built-in type (int, int64, float, bool or string). Such lists can be
// printed directly using fmt.Println without custom helpers.
func isTypedSimpleList(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		return isComparableSimple(lt.Elem)
	}
	return false
}

// isStructList reports whether t is a list of struct values.
func isStructList(t types.Type) bool {
	if lt, ok := t.(types.ListType); ok {
		if _, ok := lt.Elem.(types.StructType); ok {
			return true
		}
	}
	return false
}

func containsAny(t types.Type) bool { return types.ContainsAny(t) }

func isStringAnyMap(t types.Type) bool { return types.IsStringAnyMap(t) }

// isStringMap reports whether t is a map with string keys regardless of value type.
func isStringMap(t types.Type) bool { return types.IsStringMap(t) }

// isStringMapLike reports whether t is or resolves to a map with string keys.
// It also returns true for union types where all variants are string-keyed maps.
func isStringMapLike(t types.Type) bool { return types.IsStringMapLike(t) }

// isStringAnyMapLike reports whether t is or resolves to a map[string]any.
func isStringAnyMapLike(t types.Type) bool { return types.IsStringAnyMapLike(t) }

func isMap(t types.Type) bool    { return types.IsMapType(t) }
func isStruct(t types.Type) bool { return types.IsStructType(t) }
func isUnion(t types.Type) bool  { return types.IsUnionType(t) }
func isAny(t types.Type) bool    { return types.IsAnyType(t) }

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

	if toGo == "" {
		return expr
	}

	// If both types are the same and not `any`, no cast is needed.
	if toGo == fromGo && !isAny(from) {
		return expr
	}
	if equalTypes(from, to) && !isAny(from) && !isAny(to) {
		return expr
	}

	// Handle list conversions specially to avoid unnecessary helper calls.
	if fl, ok := from.(types.ListType); ok {
		if tl, ok := to.(types.ListType); ok {
			if equalTypes(fl.Elem, tl.Elem) {
				return fmt.Sprintf("%s(%s)", toGo, expr)
			}
			if _, ok := tl.Elem.(types.AnyType); ok {
				// converting to []any only needs a simple copy
				convPrefix := "_toAnySlice("
				normExpr := strings.ReplaceAll(strings.TrimSpace(expr), " ", "")
				if strings.HasPrefix(normExpr, convPrefix) {
					return expr
				}
				c.use("_toAnySlice")
				return fmt.Sprintf("_toAnySlice(%s)", expr)
			}
			convPrefix := fmt.Sprintf("_convSlice[%s,%s](", goType(fl.Elem), goType(tl.Elem))
			// Allow optional spaces inside the generic parameters to avoid
			// accidentally wrapping the same conversion multiple times.
			normExpr := strings.ReplaceAll(strings.TrimSpace(expr), " ", "")
			normPrefix := strings.ReplaceAll(convPrefix, " ", "")
			if strings.HasPrefix(normExpr, normPrefix) {
				return expr
			}
			c.use("_convSlice")
			return fmt.Sprintf("_convSlice[%s,%s](%s)", goType(fl.Elem), goType(tl.Elem), expr)
		}
	}

	if isString(from) {
		c.imports["strconv"] = true
		switch {
		case isInt(to):
			return fmt.Sprintf("func() int { v, _ := strconv.Atoi(%s); return v }()", expr)
		case isInt64(to):
			return fmt.Sprintf("func() int64 { v, _ := strconv.ParseInt(%s, 10, 64); return v }()", expr)
		case isFloat(to):
			return fmt.Sprintf("func() float64 { v, _ := strconv.ParseFloat(%s, 64); return v }()", expr)
		}
	}

	if isNumeric(from) && isNumeric(to) {
		return fmt.Sprintf("%s(%s)", toGo, expr)
	}

	if isBool(to) && strings.HasPrefix(fromGo, "*") {
		return fmt.Sprintf("%s != nil", expr)
	}

	if _, ok := from.(types.AnyType); ok {
		if isBool(to) {
			c.use("_exists")
			return fmt.Sprintf("_exists(%s)", expr)
		}
		if isNumeric(to) && strings.ContainsAny(expr, "+-*/%") {
			return fmt.Sprintf("%s(%s)", toGo, expr)
		}
		return fmt.Sprintf("(%s).(%s)", expr, toGo)
	}

	return fmt.Sprintf("%s(%s)", toGo, expr)
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
	return types.SimpleStringKey(e)
}

// eqJoinKeys checks if expression e represents an equality comparison between
// fields of the left and right query variables. If so it returns the compiled
// expressions for the left and right keys.
func (c *Compiler) eqJoinKeys(e *parser.Expr, leftVar, rightVar string) (string, string, bool) {
	if e == nil || len(e.Binary.Right) != 1 {
		return "", "", false
	}
	op := e.Binary.Right[0]
	if op.Op != "==" {
		return "", "", false
	}
	lhs := e.Binary.Left
	rhs := op.Right
	if len(lhs.Ops) != 0 || len(rhs.Ops) != 0 || lhs.Value == nil || rhs.Target == nil {
		return "", "", false
	}
	ls := lhs.Value.Target.Selector
	rs := rhs.Target.Selector
	if ls == nil || rs == nil {
		return "", "", false
	}
	// compile expressions
	lExpr, err1 := c.compileExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: lhs}})
	rExpr, err2 := c.compileExpr(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: rhs}}})
	if err1 != nil || err2 != nil {
		return "", "", false
	}
	if ls.Root == leftVar && rs.Root == rightVar {
		return lExpr, rExpr, true
	}
	if ls.Root == rightVar && rs.Root == leftVar {
		return rExpr, lExpr, true
	}
	return "", "", false
}

// eqJoinKeysTyped is like eqJoinKeys but also returns the static types of the
// left and right key expressions.
func (c *Compiler) eqJoinKeysTyped(e *parser.Expr, leftVar, rightVar string) (string, string, types.Type, types.Type, bool) {
	if e == nil || len(e.Binary.Right) != 1 {
		return "", "", nil, nil, false
	}
	op := e.Binary.Right[0]
	if op.Op != "==" {
		return "", "", nil, nil, false
	}
	lhs := e.Binary.Left
	rhs := op.Right
	if len(lhs.Ops) != 0 || len(rhs.Ops) != 0 || lhs.Value == nil || rhs.Target == nil {
		return "", "", nil, nil, false
	}
	ls := lhs.Value.Target.Selector
	rs := rhs.Target.Selector
	if ls == nil || rs == nil {
		return "", "", nil, nil, false
	}
	lExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: lhs}}
	rExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: rhs}}}
	lStr, err1 := c.compileExpr(lExpr)
	rStr, err2 := c.compileExpr(rExpr)
	lType := c.inferExprType(lExpr)
	rType := c.inferExprType(rExpr)
	if err1 != nil || err2 != nil {
		return "", "", nil, nil, false
	}
	if ls.Root == leftVar && rs.Root == rightVar {
		return lStr, rStr, lType, rType, true
	}
	if ls.Root == rightVar && rs.Root == leftVar {
		return rStr, lStr, rType, lType, true
	}
	return "", "", nil, nil, false
}

func (c *Compiler) newVar() string {
	name := fmt.Sprintf("tmp%d", c.tempVarCount)
	c.tempVarCount++
	return name
}

func (c *Compiler) newNamedVar(prefix string) string {
	name := fmt.Sprintf("%s%d", prefix, c.tempVarCount)
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
		return fmt.Sprintf("[]%s{}", goType(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("map[%s]%s{}", goType(tt.Key), goType(tt.Value))
	case types.StructType:
		return fmt.Sprintf("%s{}", sanitizeName(tt.Name))
	case types.OptionType:
		return "nil"
	default:
		return "nil"
	}
}

// assignStructFields copies fields from varName into itemVar as a map.
func (c *Compiler) assignStructFields(buf *bytes.Buffer, indent, itemVar, varName string, st types.StructType) {
	for _, fn := range st.Order {
		buf.WriteString(fmt.Sprintf("%s%s[\"%s\"] = %s.%s\n", indent, itemVar, fn, varName, exportName(sanitizeName(fn))))
	}
}
