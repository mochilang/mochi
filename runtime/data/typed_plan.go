package data

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

// PlanString returns a human readable representation of the plan tree.
func PlanString(pl Plan) string {
	var buf bytes.Buffer
	exprStr := func(e *parser.Expr) string {
		if e == nil {
			return ""
		}
		return strings.TrimSpace(ast.FromExpr(e).String())
	}
	var walk func(Plan, string)
	walk = func(p Plan, indent string) {
		switch n := p.(type) {
		case *scanPlan:
			fmt.Fprintf(&buf, "%sScan(alias=%s, src=%s)\n", indent, n.Alias, exprStr(n.Src))
		case *selectPlan:
			fmt.Fprintf(&buf, "%sSelect(%s)\n", indent, exprStr(n.Expr))
			walk(n.Input, indent+"  ")
		case *wherePlan:
			fmt.Fprintf(&buf, "%sWhere(%s)\n", indent, exprStr(n.Cond))
			walk(n.Input, indent+"  ")
		case *joinPlan:
			on := exprStr(n.On)
			if on == "" {
				fmt.Fprintf(&buf, "%sJoin(%s)\n", indent, n.JoinType)
			} else {
				fmt.Fprintf(&buf, "%sJoin(%s on %s)\n", indent, n.JoinType, on)
			}
			walk(n.Left, indent+"  ")
			walk(n.Right, indent+"  ")
		case *groupPlan:
			exprs := make([]string, len(n.By))
			for i, e := range n.By {
				exprs[i] = exprStr(e)
			}
			fmt.Fprintf(&buf, "%sGroup(%s by %s)\n", indent, n.Name, strings.Join(exprs, ", "))
			walk(n.Input, indent+"  ")
		case *sortPlan:
			fmt.Fprintf(&buf, "%sSort(%s)\n", indent, exprStr(n.Key))
			walk(n.Input, indent+"  ")
		case *limitPlan:
			skip := exprStr(n.Skip)
			take := exprStr(n.Take)
			switch {
			case skip != "" && take != "":
				fmt.Fprintf(&buf, "%sLimit(skip=%s, take=%s)\n", indent, skip, take)
			case skip != "":
				fmt.Fprintf(&buf, "%sLimit(skip=%s)\n", indent, skip)
			case take != "":
				fmt.Fprintf(&buf, "%sLimit(take=%s)\n", indent, take)
			default:
				fmt.Fprintf(&buf, "%sLimit\n", indent)
			}
			walk(n.Input, indent+"  ")
		}
	}
	walk(pl, "")
	return buf.String()
}
