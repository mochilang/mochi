//go:build archived

package dartcode

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

var dartReserved = map[string]struct{}{
	"import": {}, "class": {}, "void": {}, "int": {}, "double": {}, "var": {}, "for": {}, "if": {}, "else": {}, "return": {}, "true": {}, "false": {},
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
	if _, ok := dartReserved[res]; ok {
		return "_" + res
	}
	return res
}

func (c *Compiler) newVar() string {
	name := fmt.Sprintf("_tmp%d", c.tempVarCount)
	c.tempVarCount++
	return name
}

// writeVarDecl emits a typed variable declaration.
// If typ cannot be resolved to a concrete Dart type, "dynamic" is used.
func (c *Compiler) writeVarDecl(name string, typ types.Type, val string) {
	t := dartType(typ)
	if t == "" {
		t = "dynamic"
	}
	if val == "" {
		c.writeln(fmt.Sprintf("%s %s;", t, name))
	} else {
		c.writeln(fmt.Sprintf("%s %s = %s;", t, name, val))
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

func exprUsesOnlyVar(e *parser.Expr, name string) bool {
	used := map[string]bool{}

	var visitExpr func(*parser.Expr)
	var visitPrimary func(*parser.Primary)
	var visitPostfix func(*parser.PostfixExpr)
	var visitUnary func(*parser.Unary)
	var visitIf func(*parser.IfExpr)

	visitExpr = func(e *parser.Expr) {
		if e == nil {
			return
		}
		visitUnary(e.Binary.Left)
		for _, op := range e.Binary.Right {
			visitPostfix(op.Right)
		}
	}

	visitUnary = func(u *parser.Unary) {
		if u == nil {
			return
		}
		visitPostfix(u.Value)
	}

	visitPostfix = func(p *parser.PostfixExpr) {
		if p == nil {
			return
		}
		visitPrimary(p.Target)
		for _, op := range p.Ops {
			if op.Call != nil {
				for _, a := range op.Call.Args {
					visitExpr(a)
				}
			}
			if op.Index != nil {
				visitExpr(op.Index.Start)
				visitExpr(op.Index.End)
				visitExpr(op.Index.Step)
			}
		}
	}

	visitIf = func(ie *parser.IfExpr) {
		if ie == nil {
			return
		}
		visitExpr(ie.Cond)
		visitExpr(ie.Then)
		visitIf(ie.ElseIf)
		visitExpr(ie.Else)
	}

	visitPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		switch {
		case p.Selector != nil:
			used[p.Selector.Root] = true
		case p.Call != nil:
			for _, a := range p.Call.Args {
				visitExpr(a)
			}
		case p.Struct != nil:
			for _, f := range p.Struct.Fields {
				visitExpr(f.Value)
			}
		case p.List != nil:
			for _, el := range p.List.Elems {
				visitExpr(el)
			}
		case p.Map != nil:
			for _, it := range p.Map.Items {
				visitExpr(it.Key)
				visitExpr(it.Value)
			}
		case p.Group != nil:
			visitExpr(p.Group)
		case p.If != nil:
			visitIf(p.If)
		case p.Query != nil:
			visitExpr(p.Query.Select)
		}
	}

	visitExpr(e)
	for n := range used {
		if n != name && n != "_" {
			return false
		}
	}
	return true
}

func dartType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "String"
	case types.BoolType:
		return "bool"
	case types.ListType:
		elem := dartType(tt.Elem)
		if elem == "dynamic" || elem == "" {
			return "List"
		}
		return "List<" + elem + ">"
	case types.MapType:
		key := dartType(tt.Key)
		val := dartType(tt.Value)
		if key == "" {
			key = "dynamic"
		}
		if val == "" {
			val = "dynamic"
		}
		return "Map<" + key + ", " + val + ">"
	case types.GroupType:
		return "_Group"
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.VoidType:
		return "void"
	case types.AnyType:
		return "dynamic"
	case types.FuncType:
		return "dynamic"
	default:
		return "dynamic"
	}
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
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

func isBool(t types.Type) bool {
	_, ok := t.(types.BoolType)
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
	case types.StructType, types.UnionType, types.GroupType:
		return true
	default:
		return false
	}
}

func isSimpleType(t types.Type) bool {
	return isInt(t) || isInt64(t) || isFloat(t) || isBool(t) || isString(t) || isStruct(t)
}

func isAny(t types.Type) bool {
	_, ok := t.(types.AnyType)
	return ok
}

func isLiteralExpr(e *parser.Expr) bool {
	if e == nil || len(e.Binary.Right) != 0 || len(e.Binary.Left.Ops) != 0 {
		return false
	}
	p := e.Binary.Left.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil {
		return false
	}
	return p.Target.Lit != nil
}
