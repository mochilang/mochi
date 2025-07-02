package logic

import (
	"fmt"
	"strings"

	sqlparser "github.com/xwb1989/sqlparser"
)

// Format the value for Mochi source.
func formatValue(v any) string {
	switch t := v.(type) {
	case nil:
		return "null"
	case string:
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

func exprToMochi(e sqlparser.Expr) string {
	switch v := e.(type) {
	case *sqlparser.SQLVal:
		switch v.Type {
		case sqlparser.StrVal:
			return fmt.Sprintf("\"%s\"", string(v.Val))
		case sqlparser.IntVal, sqlparser.FloatVal:
			return string(v.Val)
		}
	case *sqlparser.ColName:
		return fmt.Sprintf("row[\"%s\"]", v.Name.String())
	case *sqlparser.ParenExpr:
		return "(" + exprToMochi(v.Expr) + ")"
	case *sqlparser.UnaryExpr:
		return fmt.Sprintf("%s%s", v.Operator, exprToMochi(v.Expr))
	case *sqlparser.BinaryExpr:
		l := exprToMochi(v.Left)
		r := exprToMochi(v.Right)
		return fmt.Sprintf("%s %s %s", l, v.Operator, r)
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
	default:
		return false
	}
}

func condToMochi(where *sqlparser.Where) string {
	if where == nil {
		return ""
	}
	return condExprToMochi(where.Expr)
}

func condExprToMochi(e sqlparser.Expr) string {
	switch v := e.(type) {
	case *sqlparser.ComparisonExpr:
		left := exprToMochi(v.Left)
		right := exprToMochi(v.Right)
		op := v.Operator
		if op == "=" {
			op = "=="
		}
		return fmt.Sprintf("%s %s %s", left, op, right)
	case *sqlparser.AndExpr:
		l := condExprToMochi(v.Left)
		r := condExprToMochi(v.Right)
		if l == "" || r == "" {
			return ""
		}
		return fmt.Sprintf("(%s && %s)", l, r)
	case *sqlparser.OrExpr:
		l := condExprToMochi(v.Left)
		r := condExprToMochi(v.Right)
		if l == "" || r == "" {
			return ""
		}
		return fmt.Sprintf("(%s || %s)", l, r)
	case *sqlparser.ParenExpr:
		return condExprToMochi(v.Expr)
	}
	return ""
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
		sb.WriteString(fmt.Sprintf("%srow[\"%s\"] = %s\n", indent, expr.Name.Name.String(), exprToMochi(expr.Expr)))
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
	if c.Hash != "" {
		return ""
	}
	var sb strings.Builder

	if len(c.Comments) > 0 {
		sb.WriteString("/*\n")
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
		sb.WriteString(fmt.Sprintf("%s %s = [\n", kw, name))
		for _, row := range t.Rows {
			sb.WriteString("  {\n")
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
			sb.WriteString("\n  order by " + exprToMochi(sel.OrderBy[0].Expr))
		}
		sb.WriteString("\n  select " + expr + "\n")
		sb.WriteString("for x in result {\n  print(x)\n}\n\n")
		if len(c.Expect) > 0 {
			sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == %s\n}\n", c.Name, formatExpectList(c.Expect)))
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
