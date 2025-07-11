//go:build slow

package phpcode

import (
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

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

func simpleCall(e *parser.Expr, name string) (*parser.Expr, bool) {
	call, ok := callPattern(e)
	if !ok || call.Func != name || len(call.Args) != 1 {
		return nil, false
	}
	return call.Args[0], true
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

func isSimpleIdentExpr(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Selector == nil || len(p.Target.Selector.Tail) != 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
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
	if p.Target.Map != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		if c != nil && c.needsJSON { /*no-op*/
		}
		if c != nil && c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if _, ok := t.(types.MapType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isGroupVarExpr(e *parser.Expr) (string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Selector == nil || len(p.Target.Selector.Tail) != 0 {
		return "", false
	}
	name := sanitizeName(p.Target.Selector.Root)
	if c.groupVars != nil && c.groupVars[name] {
		return name, true
	}
	return "", false
}

func isStringType(t types.Type) bool {
	_, ok := t.(types.StringType)
	return ok
}

func isMapType(t types.Type) bool {
	_, ok := t.(types.MapType)
	return ok
}

func formatList(elems []string) string {
	if len(elems) <= 1 {
		return "[" + strings.Join(elems, ", ") + "]"
	}
	var b strings.Builder
	b.WriteString("[\n")
	for i, e := range elems {
		ind := strings.ReplaceAll(e, "\n", "\n    ")
		b.WriteString("    " + ind)
		if i < len(elems)-1 {
			b.WriteString(",\n")
		} else {
			b.WriteString("\n")
		}
	}
	b.WriteString("]")
	return b.String()
}

func formatMap(items []string) string {
	if len(items) <= 1 {
		return "[" + strings.Join(items, ", ") + "]"
	}
	var b strings.Builder
	b.WriteString("[\n")
	for i, it := range items {
		ind := strings.ReplaceAll(it, "\n", "\n    ")
		b.WriteString("    " + ind)
		if i < len(items)-1 {
			b.WriteString(",\n")
		} else {
			b.WriteString("\n")
		}
	}
	b.WriteString("]")
	return b.String()
}

func (c *Compiler) use(name string) {
	if c.helpers == nil {
		c.helpers = map[string]bool{}
	}
	c.helpers[name] = true
}

func (c *Compiler) emitRuntime() {
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for i, n := range names {
		if code, ok := helperMap[n]; ok {
			if i > 0 {
				c.buf.WriteByte('\n')
			}
			c.buf.WriteString(code)
			c.buf.WriteByte('\n')
		}
	}
}

func (c *Compiler) newTmp() string {
	name := fmt.Sprintf("$_tmp%d", c.tmpCount)
	c.tmpCount++
	return name
}

func scanStmt(s *parser.Statement, vars map[string]struct{}) {
	switch {
	case s.Let != nil:
		scanExpr(s.Let.Value, vars)
	case s.Var != nil:
		scanExpr(s.Var.Value, vars)
	case s.Expect != nil:
		scanExpr(s.Expect.Value, vars)
	case s.Test != nil:
		for _, st := range s.Test.Body {
			scanStmt(st, vars)
		}
	case s.Assign != nil:
		scanExpr(s.Assign.Value, vars)
	case s.Return != nil:
		scanExpr(s.Return.Value, vars)
	case s.Expr != nil:
		scanExpr(s.Expr.Expr, vars)
	case s.For != nil:
		scanExpr(s.For.Source, vars)
		scanExpr(s.For.RangeEnd, vars)
		for _, st := range s.For.Body {
			scanStmt(st, vars)
		}
	case s.While != nil:
		scanExpr(s.While.Cond, vars)
		for _, st := range s.While.Body {
			scanStmt(st, vars)
		}
	case s.If != nil:
		scanExpr(s.If.Cond, vars)
		for _, st := range s.If.Then {
			scanStmt(st, vars)
		}
		if s.If.ElseIf != nil {
			scanStmt(&parser.Statement{If: s.If.ElseIf}, vars)
		}
		for _, st := range s.If.Else {
			scanStmt(st, vars)
		}
	}
}

func scanExpr(e *parser.Expr, vars map[string]struct{}) {
	if e == nil {
		return
	}
	scanUnary(e.Binary.Left, vars)
	for _, op := range e.Binary.Right {
		scanPostfix(op.Right, vars)
	}
}

func scanUnary(u *parser.Unary, vars map[string]struct{}) {
	if u == nil {
		return
	}
	scanPostfix(u.Value, vars)
}

func scanPostfix(p *parser.PostfixExpr, vars map[string]struct{}) {
	if p == nil {
		return
	}
	scanPrimary(p.Target, vars)
	for _, op := range p.Ops {
		if op.Index != nil {
			scanExpr(op.Index.Start, vars)
			scanExpr(op.Index.End, vars)
		}
		if op.Call != nil {
			for _, a := range op.Call.Args {
				scanExpr(a, vars)
			}
		}
	}
}

func scanPrimary(p *parser.Primary, vars map[string]struct{}) {
	if p == nil {
		return
	}
	if p.Selector != nil {
		vars[p.Selector.Root] = struct{}{}
	}
	if p.Group != nil {
		scanExpr(p.Group, vars)
	}
	if p.FunExpr != nil {
		scanExpr(p.FunExpr.ExprBody, vars)
		for _, st := range p.FunExpr.BlockBody {
			scanStmt(st, vars)
		}
	}
	if p.List != nil {
		for _, e := range p.List.Elems {
			scanExpr(e, vars)
		}
	}
	if p.Map != nil {
		for _, it := range p.Map.Items {
			if _, ok := types.SimpleStringKey(it.Key); !ok {
				scanExpr(it.Key, vars)
			}
			scanExpr(it.Value, vars)
		}
	}
	if p.Call != nil {
		for _, a := range p.Call.Args {
			scanExpr(a, vars)
		}
	}
}

func queryFreeVars(q *parser.QueryExpr, env *types.Env) []string {
	vars := map[string]struct{}{}
	scanExpr(q.Source, vars)
	for _, f := range q.Froms {
		scanExpr(f.Src, vars)
	}
	for _, j := range q.Joins {
		scanExpr(j.Src, vars)
		scanExpr(j.On, vars)
	}
	if q.Group != nil {
		scanExpr(q.Group.Exprs[0], vars)
	}
	scanExpr(q.Select, vars)
	scanExpr(q.Where, vars)
	scanExpr(q.Sort, vars)
	scanExpr(q.Skip, vars)
	scanExpr(q.Take, vars)
	delete(vars, q.Var)
	for _, f := range q.Froms {
		delete(vars, f.Var)
	}
	for _, j := range q.Joins {
		delete(vars, j.Var)
	}
	if q.Group != nil {
		delete(vars, q.Group.Name)
	}
	outMap := map[string]struct{}{}
	for k := range vars {
		if env != nil {
			if _, err := env.GetVar(k); err != nil {
				continue
			}
		}
		outMap["$"+sanitizeName(k)] = struct{}{}
	}
	out := make([]string, 0, len(outMap))
	for k := range outMap {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}
