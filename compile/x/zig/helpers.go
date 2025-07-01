package zigcode

import (
	"fmt"
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
		return fmt.Sprintf("std.AutoHashMap(%s, %s)", zigTypeOf(tt.Key), zigTypeOf(tt.Value))
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
