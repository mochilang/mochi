//go:build archived

package swiftcode

import (
	"strings"

	"mochi/parser"
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
	return s
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

func stringKeyName(e *parser.Expr) string {
	if e == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return ""
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return ""
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str
	}
	return ""
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
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}
func exprVars(e *parser.Expr, vars map[string]bool) {
	if e == nil {
		return
	}
	var scanPostfix func(p *parser.PostfixExpr)
	var scanPrimary func(p *parser.Primary)
	var scanUnaryRec func(u *parser.Unary)
	scanUnaryRec = func(u *parser.Unary) {
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
				exprVars(op.Index.Start, vars)
				exprVars(op.Index.End, vars)
			}
			if op.Call != nil {
				for _, a := range op.Call.Args {
					exprVars(a, vars)
				}
			}
		}
	}
	scanPrimary = func(p *parser.Primary) {
		if p == nil {
			return
		}
		switch {
		case p.Selector != nil:
			vars[p.Selector.Root] = true
		case p.Group != nil:
			exprVars(p.Group, vars)
		case p.FunExpr != nil:
			exprVars(p.FunExpr.ExprBody, vars)
			for _, st := range p.FunExpr.BlockBody {
				_ = st
			}
		case p.Struct != nil:
			for _, f := range p.Struct.Fields {
				exprVars(f.Value, vars)
			}
		case p.Call != nil:
			for _, a := range p.Call.Args {
				exprVars(a, vars)
			}
		case p.List != nil:
			for _, el := range p.List.Elems {
				exprVars(el, vars)
			}
		case p.Map != nil:
			for _, it := range p.Map.Items {
				exprVars(it.Key, vars)
				exprVars(it.Value, vars)
			}
		case p.Match != nil:
			exprVars(p.Match.Target, vars)
			for _, cs := range p.Match.Cases {
				exprVars(cs.Pattern, vars)
				exprVars(cs.Result, vars)
			}
		case p.Query != nil:
			vars[p.Query.Var] = true
			exprVars(p.Query.Source, vars)
			for _, f := range p.Query.Froms {
				vars[f.Var] = true
				exprVars(f.Src, vars)
			}
			for _, j := range p.Query.Joins {
				vars[j.Var] = true
				exprVars(j.Src, vars)
				exprVars(j.On, vars)
			}
			exprVars(p.Query.Where, vars)
			if p.Query.Group != nil {
				exprVars(p.Query.Group.Exprs[0], vars)
			}
			exprVars(p.Query.Sort, vars)
			exprVars(p.Query.Skip, vars)
			exprVars(p.Query.Take, vars)
			exprVars(p.Query.Select, vars)
		case p.Load != nil:
			if p.Load.Path == nil {
			} else {
			}
		case p.Save != nil:
			exprVars(p.Save.Src, vars)
		case p.Fetch != nil:
			exprVars(p.Fetch.URL, vars)
			exprVars(p.Fetch.With, vars)
		case p.Generate != nil:
			for _, f := range p.Generate.Fields {
				exprVars(f.Value, vars)
			}
		}
	}

	scanUnaryRec(e.Binary.Left)
	for _, part := range e.Binary.Right {
		scanPostfix(part.Right)
	}
}

func subsetVars(have map[string]bool, want map[string]bool) bool {
	for k := range want {
		if !have[k] {
			return false
		}
	}
	return true
}
