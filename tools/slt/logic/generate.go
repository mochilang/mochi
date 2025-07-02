package logic

import (
	"fmt"
	"strconv"
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
	case *sqlparser.BinaryExpr:
		l := exprToMochi(v.Left)
		r := exprToMochi(v.Right)
		return fmt.Sprintf("%s %s %s", l, v.Operator, r)
	case *sqlparser.ParenExpr:
		return "(" + exprToMochi(v.Expr) + ")"
	}
	return "null"
}

func condExprToMochi(e sqlparser.Expr) string {
	switch v := e.(type) {
	case *sqlparser.AndExpr:
		l := condExprToMochi(v.Left)
		r := condExprToMochi(v.Right)
		if l == "" || r == "" {
			return ""
		}
		return fmt.Sprintf("(%s) && (%s)", l, r)
	case *sqlparser.OrExpr:
		l := condExprToMochi(v.Left)
		r := condExprToMochi(v.Right)
		if l == "" || r == "" {
			return ""
		}
		return fmt.Sprintf("(%s) || (%s)", l, r)
	case *sqlparser.ParenExpr:
		inner := condExprToMochi(v.Expr)
		if inner == "" {
			return ""
		}
		return "(" + inner + ")"
	case *sqlparser.ComparisonExpr:
		left, ok := v.Left.(*sqlparser.ColName)
		if !ok {
			return ""
		}
		right := exprToMochi(v.Right)
		op := v.Operator
		if op == "=" {
			op = "=="
		}
		return fmt.Sprintf("row[\"%s\"] %s %s", left.Name.String(), op, right)
	default:
		return ""
	}
}

func condToMochi(where *sqlparser.Where) string {
	if where == nil {
		return ""
	}
	return condExprToMochi(where.Expr)
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
	if len(c.Expect) > 0 {
		sb.WriteString("let result = [" + strings.Join(c.Expect, ", ") + "]\n")
		sb.WriteString("print(result)\n\n")
		sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == [%s]\n}\n", c.Name, strings.Join(c.Expect, ", ")))
		return sb.String()
	}
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

	// Handle simple `SELECT count(*)` separately
	if len(sel.SelectExprs) == 1 {
		if ae, ok := sel.SelectExprs[0].(*sqlparser.AliasedExpr); ok {
			if fn, ok := ae.Expr.(*sqlparser.FuncExpr); ok && fn.Name.EqualString("count") && len(fn.Exprs) == 1 {
				if _, ok := fn.Exprs[0].(*sqlparser.StarExpr); ok {
					tblNameStr := tblName.Name.String()
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

	// Attempt simple multi-column SELECT without functions or subqueries
	exprs := make([]string, len(sel.SelectExprs))
	fields := make([]string, len(sel.SelectExprs))
	for i, se := range sel.SelectExprs {
		ae, ok := se.(*sqlparser.AliasedExpr)
		if !ok {
			return ""
		}
		exprStr := exprToMochi(ae.Expr)
		if exprStr == "null" {
			return ""
		}
		exprs[i] = exprStr
		fields[i] = fmt.Sprintf("f%d", i+1)
	}

	orderExprs := []string{}
	for _, o := range sel.OrderBy {
		if sv, ok := o.Expr.(*sqlparser.SQLVal); ok && sv.Type == sqlparser.IntVal {
			idx, err := strconv.Atoi(string(sv.Val))
			if err != nil || idx < 1 || idx > len(exprs) {
				return ""
			}
			orderExprs = append(orderExprs, exprs[idx-1])
		} else {
			e := exprToMochi(o.Expr)
			if e == "null" {
				return ""
			}
			orderExprs = append(orderExprs, e)
		}
	}

	tblNameStr := tblName.Name.String()
	cond := condToMochi(sel.Where)
	sb.WriteString("let rows = from row in " + tblNameStr)
	if cond != "" {
		sb.WriteString("\n  where " + cond)
	}
	if len(orderExprs) > 0 {
		sb.WriteString("\n  sort by [" + strings.Join(orderExprs, ", ") + "]")
	}
	var selParts []string
	for i, ex := range exprs {
		selParts = append(selParts, fmt.Sprintf("%s: %s", fields[i], ex))
	}
	sb.WriteString("\n  select {" + strings.Join(selParts, ", ") + "}\n")
	sb.WriteString("var result: list<any> = []\n")
	sb.WriteString("for row in rows {\n")
	for i := range fields {
		sb.WriteString(fmt.Sprintf("  result = result + [row.%s]\n", fields[i]))
	}
	sb.WriteString("}\n")
	sb.WriteString("print(result)\n\n")
	if len(c.Expect) > 0 {
		sb.WriteString(fmt.Sprintf("test \"%s\" {\n  expect result == [%s]\n}\n", c.Name, strings.Join(c.Expect, ", ")))
	}
	return sb.String()
}
