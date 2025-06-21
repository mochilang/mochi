package fscode

import (
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func fsType(t *parser.TypeRef) string {
	if t == nil {
		return "obj"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "int"
		case "float":
			return "float"
		case "bool":
			return "bool"
		case "string":
			return "string"
		case "void":
			return "unit"
		default:
			return sanitizeName(*t.Simple)
		}
	}
	if t.Fun != nil {
		ret := "unit"
		if t.Fun.Return != nil {
			ret = fsType(t.Fun.Return)
		}
		for i := len(t.Fun.Params) - 1; i >= 0; i-- {
			ret = fmt.Sprintf("%s -> %s", fsType(t.Fun.Params[i]), ret)
		}
		return ret
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return fsType(t.Generic.Args[0]) + "[]"
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return fmt.Sprintf("Map<%s,%s>", fsType(t.Generic.Args[0]), fsType(t.Generic.Args[1]))
		}
	}
	return "obj"
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
		s = "_" + s
	}
	switch s {
	case "abstract", "and", "as", "assert", "base", "begin", "class", "default",
		"delegate", "do", "done", "downcast", "downto", "elif", "else", "end",
		"exception", "extern", "false", "finally", "for", "fun", "function",
		"if", "in", "inherit", "inline", "interface", "internal", "lazy", "let",
		"match", "member", "module", "mutable", "namespace", "new", "null", "of",
		"open", "or", "override", "private", "public", "rec", "return", "sig",
		"static", "struct", "then", "to", "true", "try", "type", "upcast", "use",
		"val", "void", "when", "while", "with", "yield":
		s = "_" + s
	}
	return s
}

func isSimpleIdent(s string) bool {
	if s == "" {
		return false
	}
	for i, r := range s {
		if !(r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z')) {
			return false
		}
	}
	return true
}

// --- helpers ---

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.ListType)
	return ok
}

func (c *Compiler) isListBinary(b *parser.BinaryExpr) bool {
	_, ok := c.inferBinaryType(b).(types.ListType)
	return ok
}

func (c *Compiler) isListUnary(u *parser.Unary) bool {
	_, ok := c.inferUnaryType(u).(types.ListType)
	return ok
}

func (c *Compiler) isListPostfix(p *parser.PostfixExpr) bool {
	_, ok := c.inferPostfixType(p).(types.ListType)
	return ok
}

func (c *Compiler) isListPrimary(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.ListType)
	return ok
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
	if len(p.Ops) != 0 {
		return false
	}
	return p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
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

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.StringType)
	return ok
}

func (c *Compiler) isStringBinary(b *parser.BinaryExpr) bool {
	_, ok := c.inferBinaryType(b).(types.StringType)
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

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.StringType)
	return ok
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	_, ok := c.inferExprType(e).(types.MapType)
	return ok
}

func (c *Compiler) isMapBinary(b *parser.BinaryExpr) bool {
	_, ok := c.inferBinaryType(b).(types.MapType)
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

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	_, ok := c.inferPrimaryType(p).(types.MapType)
	return ok
}

func intLiteral(e *parser.Expr) (int, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return 0, false
	}
	u := e.Binary.Left
	negate := false
	if len(u.Ops) > 0 {
		if len(u.Ops) == 1 && u.Ops[0] == "-" {
			negate = true
		} else {
			return 0, false
		}
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Lit == nil || p.Target.Lit.Int == nil {
		return 0, false
	}
	v := *p.Target.Lit.Int
	if negate {
		v = -v
	}
	return v, true
}

func extractLiteral(e *parser.Expr) *parser.Literal {
	if e == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return nil
	}
	if p.Target != nil {
		return p.Target.Lit
	}
	return nil
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func (c *Compiler) use(name string) {
	switch name {
	case "_json", "_to_json":
		name = "_json_helpers"
	}
	c.helpers[name] = true
}

func (c *Compiler) emitRuntime() {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	seen := map[string]bool{}
	for _, n := range names {
		src := helperMap[n]
		if seen[src] {
			continue
		}
		seen[src] = true
		for _, line := range strings.Split(src, "\n") {
			c.preamble.WriteString(line)
			c.preamble.WriteByte('\n')
		}
	}
}
