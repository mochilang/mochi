package cpp

import (
	"strings"

	"mochi/parser"
)

// toPascalCase converts snake_case or lowercase names to PascalCase.
func toPascalCase(s string) string {
	parts := strings.Split(s, "_")
	for i, p := range parts {
		if len(p) == 0 {
			continue
		}
		parts[i] = strings.ToUpper(p[:1]) + p[1:]
	}
	return strings.Join(parts, "")
}

// structNameFromVar derives a struct name from a variable name using a simple
// singularization heuristic.
func structNameFromVar(name string) string {
	if strings.HasSuffix(name, "ies") && len(name) > 3 {
		name = name[:len(name)-3] + "y"
	} else if strings.HasSuffix(name, "s") && len(name) > 1 {
		name = name[:len(name)-1]
	}
	return toPascalCase(name)
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) != 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

// sumOverVar returns the variable name if the expression is the builtin
// sum called on that variable.
func sumOverVar(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	pf := u.Value
	if pf == nil || pf.Target == nil || pf.Target.Call == nil || len(pf.Ops) != 0 {
		return "", false
	}
	call := pf.Target.Call
	if call.Func != "sum" || len(call.Args) != 1 {
		return "", false
	}
	if v, ok := identName(call.Args[0]); ok {
		return v, true
	}
	return "", false
}

// callPattern returns the call expression if e is a direct
// function call with no operators.
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
