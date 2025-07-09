//go:build slow

package tscode

import (
	"fmt"
	"reflect"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

var tsReserved = map[string]struct{}{
	// JavaScript/TypeScript keywords
	"break": {}, "case": {}, "catch": {}, "class": {}, "const": {}, "continue": {},
	"debugger": {}, "default": {}, "delete": {}, "do": {}, "else": {}, "enum": {},
	"export": {}, "extends": {}, "false": {}, "finally": {}, "for": {}, "function": {},
	"if": {}, "import": {}, "in": {}, "instanceof": {}, "new": {}, "null": {},
	"return": {}, "super": {}, "switch": {}, "this": {}, "throw": {}, "true": {},
	"try": {}, "typeof": {}, "var": {}, "void": {}, "while": {}, "with": {},
	"yield": {}, "let": {}, "interface": {}, "package": {}, "private": {},
	"protected": {}, "public": {}, "static": {}, "await": {}, "implements": {},
	"arguments": {}, "eval": {},
	// Avoid collision with DOM types
	"Node": {},
}

const indentStr = "  "

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString(indentStr)
	}
}

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat(indentStr, depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
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

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("type %s = {", name))
	c.indent++
	for _, fn := range st.Order {
		if typ, ok := st.Fields[fn]; ok {
			ts := tsType(typ)
			if ts != "" {
				c.writeln(fmt.Sprintf("%s: %s;", sanitizeName(fn), ts))
				continue
			}
		}
		c.writeln(fmt.Sprintf("%s: any;", sanitizeName(fn)))
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func unexportName(name string) string {
	if name == "" {
		return ""
	}
	runes := []rune(name)
	if runes[0] >= 'A' && runes[0] <= 'Z' {
		runes[0] = runes[0] - 'A' + 'a'
	}
	return string(runes)
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
	if _, ok := tsReserved[res]; ok {
		return "_" + res
	}
	return res
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

func isString(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isList(t types.Type) bool {
	_, ok := t.(types.ListType)
	return ok
}

func isMap(t types.Type) bool {
	_, ok := t.(types.MapType)
	return ok
}

func isStruct(t types.Type) bool {
	switch t.(type) {
	case types.StructType, types.UnionType:
		return true
	default:
		return false
	}
}

func tsType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type, types.FloatType:
		return "number"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "boolean"
	case types.ListType:
		elem := tsType(tt.Elem)
		// Use [] notation for simple array types to improve readability.
		if elem == "any" {
			return "any[]"
		}
		return elem + "[]"
	case types.MapType:
		return "Record<" + tsType(tt.Key) + ", " + tsType(tt.Value) + ">"
	case types.StructType:
		name := sanitizeName(tt.Name)
		if name != "" {
			return name
		}
		parts := make([]string, 0, len(tt.Fields))
		order := tt.Order
		if len(order) == 0 {
			for fn := range tt.Fields {
				order = append(order, fn)
			}
			sort.Strings(order)
		}
		for _, fn := range order {
			if ft, ok := tt.Fields[fn]; ok {
				parts = append(parts, fmt.Sprintf("%s: %s", sanitizeName(fn), tsType(ft)))
			}
		}
		return "{ " + strings.Join(parts, "; ") + " }"
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.FuncType:
		var args []string
		for i, p := range tt.Params {
			arg := fmt.Sprintf("p%d: %s", i, tsType(p))
			if tt.Variadic && i == len(tt.Params)-1 {
				arg = "..." + arg
			}
			args = append(args, arg)
		}
		ret := tsType(tt.Return)
		return fmt.Sprintf("(%s) => %s", strings.Join(args, ", "), ret)
	case types.VoidType:
		return "void"
	case types.AnyType:
		return "any"
	default:
		return "any"
	}
}

func isAny(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

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
