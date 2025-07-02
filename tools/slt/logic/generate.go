package logic

import (
	"fmt"
	"strconv"
	"strings"

	sqlparser "github.com/xwb1989/sqlparser"
)

// subqueryToMochi converts supported subqueries to Mochi source. The outer
// parameter names the variable for the outer query row when handling correlated
// predicates.
func subqueryToMochi(sub *sqlparser.Subquery, outer string) string {
	sel, ok := sub.Select.(*sqlparser.Select)
	if !ok || sel == nil || len(sel.From) != 1 || len(sel.SelectExprs) != 1 {
		return "null"
	}
	tblExpr, ok := sel.From[0].(*sqlparser.AliasedTableExpr)
	if !ok {
		return "null"
	}
	tblName, ok := tblExpr.Expr.(sqlparser.TableName)
	if !ok {
		return "null"
	}
	alias := tblExpr.As.String()
	if alias == "" {
		alias = "x"
	}
	ae, ok := sel.SelectExprs[0].(*sqlparser.AliasedExpr)
	if !ok {
		return "null"
	}
	if fn, ok := ae.Expr.(*sqlparser.FuncExpr); ok {
		name := strings.ToLower(fn.Name.String())
		if name == "count" && len(fn.Exprs) == 1 {
			if _, ok := fn.Exprs[0].(*sqlparser.StarExpr); ok {
				cond := ""
				if sel.Where != nil {
					cond = condExprToMochiRow(sel.Where.Expr, alias, outer)
				}
				var sb strings.Builder
				sb.WriteString("count(from " + alias + " in " + tblName.Name.String())
				if cond != "" {
					sb.WriteString("\n  where " + cond)
				}
				sb.WriteString("\n  select " + alias + ")")
				return sb.String()
			}
		} else if name == "avg" && len(fn.Exprs) == 1 {
			if a, ok := fn.Exprs[0].(*sqlparser.AliasedExpr); ok {
				val := exprToMochiRow(a.Expr, alias, outer)
				cond := ""
				if sel.Where != nil {
					cond = condExprToMochiRow(sel.Where.Expr, alias, outer)
				}
				var sb strings.Builder
				sb.WriteString("avg(from " + alias + " in " + tblName.Name.String())
				if cond != "" {
					sb.WriteString("\n  where " + cond)
				}
				sb.WriteString("\n  select " + val + ")")
				return sb.String()
			}
		}
	}
	return "null"
}

func existsToMochi(sub *sqlparser.Subquery, outer string) string {
	sel, ok := sub.Select.(*sqlparser.Select)
	if !ok || sel == nil || len(sel.From) != 1 {
		return ""
	}
	tblExpr, ok := sel.From[0].(*sqlparser.AliasedTableExpr)
	if !ok {
		return ""
	}
	tblName, ok := tblExpr.Expr.(sqlparser.TableName)
	if !ok {
		return ""
	}
	alias := tblExpr.As.String()
	if alias == "" {
		alias = "x"
	}
	cond := ""
	if sel.Where != nil {
		cond = condExprToMochiRow(sel.Where.Expr, alias, outer)
	}
	var sb strings.Builder
	sb.WriteString("count(from " + alias + " in " + tblName.Name.String())
	if cond != "" {
		sb.WriteString("\n  where " + cond)
	}
	sb.WriteString("\n  select " + alias + ") > 0")
	return sb.String()
}

// Format the value for Mochi source.
func formatValue(v any) string {
	switch t := v.(type) {
	case nil:
		return "null"
	case string:
		if t == "true" || t == "false" {
			return fmt.Sprintf("\"%s\" + \"\"", t)
		}
		return fmt.Sprintf("\"%s\"", t)
	case int:
		return fmt.Sprintf("%d", t)
	case float64:
		s := fmt.Sprintf("%g", t)
		if !strings.ContainsRune(s, '.') {
			s += ".0"
		}
		return s
	default:
		return fmt.Sprintf("%v", t)
	}
}

func detectColumnType(rows []map[string]any, name string) string {
	t := ""
	for _, row := range rows {
		v := row[name]
		if v == nil {
			continue
		}
		switch v.(type) {
		case string:
			if t == "" || t == "string" {
				t = "string"
			} else {
				return "any"
			}
		case float64:
			if t == "" || t == "float" || t == "int" {
				t = "float"
			} else {
				return "any"
			}
		case int:
			if t == "" {
				t = "int"
			} else if t != "int" {
				if t == "float" {
					// keep float
				} else {
					return "any"
				}
			}
		case bool:
			if t == "" || t == "bool" {
				t = "bool"
			} else {
				return "any"
			}
		default:
			return "any"
		}
	}
	if t == "" {
		return "any"
	}
	return t
}

func exprToMochi(e sqlparser.Expr) string {
	return exprToMochiRow(e, "row", "")
}

func exprToMochiRow(e sqlparser.Expr, rowVar, outer string) string {
	switch v := e.(type) {
	case *sqlparser.SQLVal:
		switch v.Type {
		case sqlparser.StrVal:
			return fmt.Sprintf("\"%s\"", string(v.Val))
		case sqlparser.IntVal, sqlparser.FloatVal:
			return string(v.Val)
		}
	case *sqlparser.ColName:
		name := v.Name.String()
		if v.Qualifier.Name.String() != "" && v.Qualifier.Name.String() != rowVar {
			if outer != "" {
				return fmt.Sprintf("%s.%s", outer, name)
			}
		}
		return fmt.Sprintf("%s.%s", rowVar, name)
	case *sqlparser.ParenExpr:
		return "(" + exprToMochiRow(v.Expr, rowVar, outer) + ")"
	case *sqlparser.UnaryExpr:
		return fmt.Sprintf("%s%s", v.Operator, exprToMochiRow(v.Expr, rowVar, outer))
	case *sqlparser.BinaryExpr:
		l := exprToMochiRow(v.Left, rowVar, outer)
		r := exprToMochiRow(v.Right, rowVar, outer)
		return fmt.Sprintf("%s %s %s", l, v.Operator, r)
	case *sqlparser.FuncExpr:
		name := strings.ToLower(v.Name.String())
		if name == "abs" && len(v.Exprs) == 1 {
			arg := v.Exprs[0].(*sqlparser.AliasedExpr).Expr
			ex := exprToMochiRow(arg, rowVar, outer)
			return fmt.Sprintf("(if %s < 0 { -(%s) } else { %s })", ex, ex, ex)
		}
	case *sqlparser.Subquery:
		return subqueryToMochi(v, rowVar)
	case *sqlparser.CaseExpr:
		elseExpr := "null"
		if v.Else != nil {
			elseExpr = exprToMochiRow(v.Else, rowVar, outer)
		}
		out := elseExpr
		if v.Expr != nil {
			expr := exprToMochiRow(v.Expr, rowVar, outer)
			for i := len(v.Whens) - 1; i >= 0; i-- {
				cond := fmt.Sprintf("%s == %s", expr, exprToMochiRow(v.Whens[i].Cond, rowVar, outer))
				val := exprToMochiRow(v.Whens[i].Val, rowVar, outer)
				out = fmt.Sprintf("(if %s { %s } else { %s })", cond, val, out)
			}
		} else {
			for i := len(v.Whens) - 1; i >= 0; i-- {
				cond := condExprToMochiRow(v.Whens[i].Cond, rowVar, outer)
				val := exprToMochiRow(v.Whens[i].Val, rowVar, outer)
				if cond == "" {
					return "null"
				}
				out = fmt.Sprintf("(if %s { %s } else { %s })", cond, val, out)
			}
		}
		return out
	}
	return "null"
}

func simpleExpr(e sqlparser.Expr) bool {
	switch v := e.(type) {
	case *sqlparser.ColName, *sqlparser.SQLVal:
		return true
	case *sqlparser.ParenExpr:
		return simpleExpr(v.Expr)
	case *sqlparser.BinaryExpr:
		return simpleExpr(v.Left) && simpleExpr(v.Right)
	case *sqlparser.UnaryExpr:
		return simpleExpr(v.Expr)
	case *sqlparser.FuncExpr:
		name := strings.ToLower(v.Name.String())
		if name == "abs" && len(v.Exprs) == 1 {
			if ae, ok := v.Exprs[0].(*sqlparser.AliasedExpr); ok {
				return simpleExpr(ae.Expr)
			}
		}
		return false
	case *sqlparser.Subquery:
		return subqueryToMochi(v, "row") != "null"
	case *sqlparser.CaseExpr:
		if v.Expr != nil {
			if !simpleExpr(v.Expr) {
				return false
			}
			for _, w := range v.Whens {
				if !simpleExpr(w.Cond) || !simpleExpr(w.Val) {
					return false
				}
			}
		} else {
			for _, w := range v.Whens {
				if condExprToMochi(w.Cond) == "" || !simpleExpr(w.Val) {
					return false
				}
			}
		}
		if v.Else != nil && !simpleExpr(v.Else) {
			return false
		}
		return true
	default:
		return false
	}
}

func condToMochi(where *sqlparser.Where) string {
	if where == nil {
		return ""
	}
	return condExprToMochiRow(where.Expr, "row", "")
}

func condExprToMochi(e sqlparser.Expr) string {
	return condExprToMochiRow(e, "row", "")
}

func condExprToMochiRow(e sqlparser.Expr, rowVar, outer string) string {
	switch v := e.(type) {
	case *sqlparser.ComparisonExpr:
		left := exprToMochiRow(v.Left, rowVar, outer)
		right := exprToMochiRow(v.Right, rowVar, outer)
		op := v.Operator
		if op == "=" {
			op = "=="
		}
		return fmt.Sprintf("%s %s %s", left, op, right)
	case *sqlparser.AndExpr:
		l := condExprToMochiRow(v.Left, rowVar, outer)
		r := condExprToMochiRow(v.Right, rowVar, outer)
		if l == "" || r == "" {
			return ""
		}
		return fmt.Sprintf("(%s && %s)", l, r)
	case *sqlparser.OrExpr:
		l := condExprToMochiRow(v.Left, rowVar, outer)
		r := condExprToMochiRow(v.Right, rowVar, outer)
		if l == "" || r == "" {
			return ""
		}
		return fmt.Sprintf("(%s || %s)", l, r)
	case *sqlparser.RangeCond:
		left := exprToMochiRow(v.Left, rowVar, outer)
		from := exprToMochiRow(v.From, rowVar, outer)
		to := exprToMochiRow(v.To, rowVar, outer)
		switch strings.ToLower(v.Operator) {
		case sqlparser.BetweenStr:
			return fmt.Sprintf("(%s >= %s && %s <= %s)", left, from, left, to)
		case sqlparser.NotBetweenStr:
			return fmt.Sprintf("(%s < %s || %s > %s)", left, from, left, to)
		}
		return ""
	case *sqlparser.ExistsExpr:
		e := existsToMochi(v.Subquery, rowVar)
		if e == "" {
			return ""
		}
		return e
	case *sqlparser.ParenExpr:
		inner := condExprToMochiRow(v.Expr, rowVar, outer)
		if inner == "" {
			return ""
		}
		return "(" + inner + ")"
	}
	return ""
}

func orderExprToMochi(e sqlparser.Expr, exprs []*sqlparser.AliasedExpr) string {
	if val, ok := e.(*sqlparser.SQLVal); ok && val.Type == sqlparser.IntVal {
		idx, err := strconv.Atoi(string(val.Val))
		if err == nil && idx >= 1 && idx <= len(exprs) {
			return exprToMochi(exprs[idx-1].Expr)
		}
	}
	return exprToMochi(e)
}

func generateUpdate(stmt string) string {
	node, err := sqlparser.Parse(stmt)
	if err != nil {
		return ""
	}
	u := node.(*sqlparser.Update)
	tbl := u.TableExprs[0].(*sqlparser.AliasedTableExpr).Expr.(sqlparser.TableName).Name.String()
	cond := condToMochi(u.Where)
	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("var i = 0\nwhile i < len(%s) {\n", tbl))
	sb.WriteString(fmt.Sprintf("  var row = %s[i]\n", tbl))
	indent := "  "
	if cond != "" {
		sb.WriteString(fmt.Sprintf("  if %s {\n", cond))
		indent = "    "
	}
	for _, expr := range u.Exprs {
		sb.WriteString(fmt.Sprintf("%srow.%s = %s\n", indent, expr.Name.Name.String(), exprToMochi(expr.Expr)))
	}
	sb.WriteString(fmt.Sprintf("%s%s[i] = row\n", indent, tbl))
	if cond != "" {
		sb.WriteString("  }\n")
	}
	sb.WriteString("  i = i + 1\n}\n\n")
	return sb.String()
}

// Generate returns Mochi source code for the given Case.
func Generate(c Case) string {
	var sb strings.Builder

	if len(c.Comments) > 0 || c.Line > 0 {
		sb.WriteString("/*\n")
		if c.Line > 0 {
			sb.WriteString(fmt.Sprintf("# line: %d\n", c.Line))
		}
		for _, line := range c.Comments {
			sb.WriteString(line + "\n")
		}
		sb.WriteString("*/\n\n")
	}

	mutated := map[string]bool{}
	for _, stmt := range c.Updates {
		node, err := sqlparser.Parse(stmt)
		if err == nil {
			u := node.(*sqlparser.Update)
			tbl := u.TableExprs[0].(*sqlparser.AliasedTableExpr).Expr.(sqlparser.TableName).Name.String()
			mutated[tbl] = true
		}
	}

	for name, t := range c.Tables {
		kw := "let"
		if mutated[name] {
			kw = "var"
		}
		typeName := name + "Row"
		sb.WriteString(fmt.Sprintf("type %s {\n", typeName))
		for _, col := range t.Columns {
			ct := detectColumnType(t.Rows, col)
			sb.WriteString(fmt.Sprintf("  %s: %s\n", col, ct))
		}
		sb.WriteString("}\n\n")

		sb.WriteString(fmt.Sprintf("%s %s = [\n", kw, name))
		for _, row := range t.Rows {
			sb.WriteString(fmt.Sprintf("  %s {\n", typeName))
			for _, col := range t.Columns {
				sb.WriteString(fmt.Sprintf("    %s: %s,\n", col, formatValue(row[col])))
			}
			sb.WriteString("  },\n")
		}
		sb.WriteString("]\n\n")
	}

	for _, u := range c.Updates {
		sb.WriteString(fmt.Sprintf("/* %s */\n", u))
		sb.WriteString(generateUpdate(u))
	}

	sb.WriteString(fmt.Sprintf("/* %s */\n", c.Query))
	stmt, err := sqlparser.Parse(c.Query)
	if err != nil {
		return ""
	}
	sel, ok := stmt.(*sqlparser.Select)
	if !ok || len(sel.From) != 1 {
		return ""
	}
	tblExpr, ok := sel.From[0].(*sqlparser.AliasedTableExpr)
	if !ok {
		return ""
	}
	tblName, ok := tblExpr.Expr.(sqlparser.TableName)
	if !ok {
		return ""
	}

	tblNameStr := tblName.Name.String()

	// Handle SELECT count(*)
	if len(sel.SelectExprs) == 1 {
		if ae, ok := sel.SelectExprs[0].(*sqlparser.AliasedExpr); ok {
			if fn, ok := ae.Expr.(*sqlparser.FuncExpr); ok && fn.Name.EqualString("count") && len(fn.Exprs) == 1 {
				if _, ok := fn.Exprs[0].(*sqlparser.StarExpr); ok {
					cond := condToMochi(sel.Where)
					sb.WriteString("let result = count(from row in " + tblNameStr)
					if cond != "" {
						sb.WriteString("\n  where " + cond)
					}
					sb.WriteString("\n  select row)\n")
					sb.WriteString("print(result)\n\n")
					if len(c.Expect) > 0 {
						sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == %s\n}\n", c.Name, c.Expect[0]))
					}
					return sb.String()
				}
			}
		}
	}

	// Support simple single-column selects
	if len(sel.SelectExprs) == 1 && len(sel.OrderBy) <= 1 {
		ae, ok := sel.SelectExprs[0].(*sqlparser.AliasedExpr)
		if !ok || !simpleExpr(ae.Expr) {
			return ""
		}
		if sel.Where != nil && condExprToMochi(sel.Where.Expr) == "" {
			return ""
		}
		if len(sel.OrderBy) == 1 && !simpleExpr(sel.OrderBy[0].Expr) {
			return ""
		}
		expr := exprToMochi(ae.Expr)
		cond := condToMochi(sel.Where)
		sb.WriteString("let result = from row in " + tblNameStr)
		if cond != "" {
			sb.WriteString("\n  where " + cond)
		}
		if len(sel.OrderBy) == 1 {
			sb.WriteString("\n  order by " + orderExprToMochi(sel.OrderBy[0].Expr, []*sqlparser.AliasedExpr{ae}))
		}
		sb.WriteString("\n  select " + expr + "\n")
		sb.WriteString("for x in result {\n  print(x)\n}\n\n")
		if len(c.Expect) > 0 {
			sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == %s\n}\n", c.Name, formatExpectList(c.Expect)))
		}
		return sb.String()
	}

	// Support simple multi-column selects without subqueries or functions
	if len(sel.SelectExprs) > 1 {
		if len(c.Expect) == 0 {
			return ""
		}
		aes := make([]*sqlparser.AliasedExpr, 0, len(sel.SelectExprs))
		var exprs []string
		for _, se := range sel.SelectExprs {
			ae, ok := se.(*sqlparser.AliasedExpr)
			if !ok || !simpleExpr(ae.Expr) {
				return ""
			}
			aes = append(aes, ae)
			exprs = append(exprs, exprToMochi(ae.Expr))
		}
		if sel.Where != nil && condExprToMochi(sel.Where.Expr) == "" {
			return ""
		}
		for _, ob := range sel.OrderBy {
			if !simpleExpr(ob.Expr) {
				if val, ok := ob.Expr.(*sqlparser.SQLVal); !ok || val.Type != sqlparser.IntVal {
					return ""
				}
			}
		}
		cond := condToMochi(sel.Where)
		sb.WriteString("let result = from row in " + tblNameStr)
		if cond != "" {
			sb.WriteString("\n  where " + cond)
		}
		if len(sel.OrderBy) > 0 {
			sb.WriteString("\n  order by ")
			if len(sel.OrderBy) == 1 {
				sb.WriteString(orderExprToMochi(sel.OrderBy[0].Expr, aes))
			} else {
				sb.WriteString("[")
				for i, ob := range sel.OrderBy {
					if i > 0 {
						sb.WriteString(", ")
					}
					sb.WriteString(orderExprToMochi(ob.Expr, aes))
				}
				sb.WriteString("]")
			}
		}
		sb.WriteString("\n  select [" + strings.Join(exprs, ", ") + "]\n")
		sb.WriteString("var flatResult = []\n")
		sb.WriteString("for row in result {\n")
		sb.WriteString("  for x in row {\n")
		sb.WriteString("    flatResult = append(flatResult, x)\n")
		sb.WriteString("  }\n}\n")
		sb.WriteString("for x in flatResult {\n  print(x)\n}\n\n")
		if len(c.Expect) > 0 {
			sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect flatResult == %s\n}\n", c.Name, formatExpectList(c.Expect)))
		}
		return sb.String()
	}

	return ""
}

func formatExpectList(xs []string) string {
	var sb strings.Builder
	sb.WriteString("[")
	for i, x := range xs {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(x)
	}
	sb.WriteString("]")
	return sb.String()
}
