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
	case *sqlparser.BinaryExpr:
		l := exprToMochi(v.Left)
		r := exprToMochi(v.Right)
		return fmt.Sprintf("%s %s %s", l, v.Operator, r)
	}
	return "null"
}

func condToMochi(where *sqlparser.Where) string {
	if where == nil {
		return ""
	}
	cmp, ok := where.Expr.(*sqlparser.ComparisonExpr)
	if !ok {
		return ""
	}
	left := cmp.Left.(*sqlparser.ColName).Name.String()
	right := exprToMochi(cmp.Right)
	op := cmp.Operator
	if op == "=" {
		op = "=="
	}
	return fmt.Sprintf("row[\"%s\"] %s %s", left, op, right)
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

	// Only support `SELECT count(*) FROM tbl WHERE ...` queries
	if len(sel.SelectExprs) != 1 {
		return ""
	}
	ae, ok := sel.SelectExprs[0].(*sqlparser.AliasedExpr)
	if !ok {
		return ""
	}
	fn, ok := ae.Expr.(*sqlparser.FuncExpr)
	if !ok || !fn.Name.EqualString("count") || len(fn.Exprs) != 1 {
		return ""
	}
	if _, ok := fn.Exprs[0].(*sqlparser.StarExpr); !ok {
		return ""
	}

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
