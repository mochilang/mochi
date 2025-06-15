package pycode

// Helper functions used by the Python compiler. Splitting them into this file
// keeps compiler.go focused on the high level logic.

import (
	"reflect"
	"strings"

	"mochi/parser"
	"mochi/types"
)

var pyReserved = map[string]struct{}{
	// Python keywords
	"False": {}, "None": {}, "True": {}, "and": {}, "as": {}, "assert": {},
	"async": {}, "await": {}, "break": {}, "class": {}, "continue": {},
	"def": {}, "del": {}, "elif": {}, "else": {}, "except": {},
	"finally": {}, "for": {}, "from": {}, "global": {}, "if": {},
	"import": {}, "in": {}, "is": {}, "lambda": {}, "nonlocal": {},
	"not": {}, "or": {}, "pass": {}, "raise": {}, "return": {},
	"try": {}, "while": {}, "with": {}, "yield": {},
	// Builtins that commonly appear in generated code
	"sorted": {},
}

func (c *Compiler) writeln(s string) { c.writeIndent(); c.buf.WriteString(s); c.buf.WriteByte('\n') }

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
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
	out := b.String()
	if _, ok := pyReserved[out]; ok {
		return "_" + out
	}
	return out
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

func isAny(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

func pyType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "float"
	case types.StringType:
		return "str"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return "list[" + pyType(tt.Elem) + "]"
	case types.MapType:
		return "dict[" + pyType(tt.Key) + ", " + pyType(tt.Value) + "]"
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.FuncType:
		return "typing.Callable"
	case types.VoidType:
		return "None"
	case types.AnyType:
		return "typing.Any"
	default:
		return "typing.Any"
	}
}

func resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return types.ListType{Elem: resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return types.MapType{Key: resolveTypeRef(args[0]), Value: resolveTypeRef(args[1])}
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
			return types.AnyType{}
		}
	}
	return types.AnyType{}
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
