//go:build archived

package rscode

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

func rustType(t *parser.TypeRef) string {
	if t == nil {
		return "()"
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return "i64"
		case "int64":
			return "i64"
		case "float":
			return "f64"
		case "string":
			return "String"
		case "bool":
			return "bool"
		default:
			return sanitizeName(*t.Simple)
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return fmt.Sprintf("Vec<%s>", rustType(t.Generic.Args[0]))
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			k := rustType(t.Generic.Args[0])
			v := rustType(t.Generic.Args[1])
			return fmt.Sprintf("std::collections::HashMap<%s, %s>", k, v)
		}
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = rustType(p)
		}
		ret := "()"
		if t.Fun.Return != nil {
			ret = rustType(t.Fun.Return)
		}
		return fmt.Sprintf("Box<dyn Fn(%s) -> %s>", strings.Join(params, ", "), ret)
	}
	return "()"
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) > 0 {
		return false
	}
	if p.Target == nil || p.Target.Lit == nil || p.Target.Lit.Str == nil {
		return false
	}
	return true
}

func (c *Compiler) isListExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil || len(p.Ops) > 0 {
		return false
	}
	if p.Target.List != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.ListType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isMapExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil || len(p.Ops) > 0 {
		return false
	}
	if p.Target.Map != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isStringExpr(p *parser.PostfixExpr) bool {
	if p == nil || p.Target == nil {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
				if lt, ok := t.(types.ListType); ok {
					if _, ok := lt.Elem.(types.StringType); ok && len(p.Ops) > 0 && p.Ops[len(p.Ops)-1].Index != nil {
						return true
					}
				}
			}
		}
	}
	return false
}

func (c *Compiler) isMapVar(name string) bool {
	if c.env == nil {
		return false
	}
	if t, err := c.env.GetVar(name); err == nil {
		if _, ok := t.(types.MapType); ok {
			return true
		}
	}
	return false
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
	res := b.String()
	if res == "" || !((res[0] >= 'A' && res[0] <= 'Z') || (res[0] >= 'a' && res[0] <= 'z') || res[0] == '_') {
		res = "_" + res
	}
	return res
}

func indentBlock(s string, depth int) string {
	if s == "" {
		return s
	}
	prefix := strings.Repeat("    ", depth)
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	for i, line := range lines {
		lines[i] = prefix + line
	}
	return strings.Join(lines, "\n") + "\n"
}

func usedAliases(e *parser.Expr) map[string]struct{} {
	aliases := map[string]struct{}{}
	if e == nil {
		return aliases
	}
	node := ast.FromExpr(e)
	var walk func(n *ast.Node)
	walk = func(n *ast.Node) {
		if n.Kind == "selector" {
			base := n
			for len(base.Children) == 1 && base.Children[0].Kind == "selector" {
				base = base.Children[0]
			}
			if s, ok := base.Value.(string); ok {
				aliases[s] = struct{}{}
			}
		}
		for _, c := range n.Children {
			walk(c)
		}
	}
	walk(node)
	return aliases
}

// compileIterExpr compiles an expression that will be iterated over. If the
// expression yields a group value, its underlying item slice is returned.
func (c *Compiler) compileIterExpr(e *parser.Expr) (string, error) {
	expr, err := c.compileExpr(e)
	if err != nil {
		return "", err
	}
	if _, ok := c.inferExprType(e).(types.GroupType); ok {
		expr += ".items"
	}
	return expr, nil
}
