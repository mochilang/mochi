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

// Generate returns Mochi source code for the given Case.
func Generate(c Case) string {
	var sb strings.Builder
	for _, line := range c.Comments {
		sb.WriteString("// " + line + "\n")
	}
	if len(c.Comments) > 0 {
		sb.WriteString("\n")
	}
	sb.WriteString("// SQL: " + c.Query + "\n")
	sb.WriteString("\n")
	for name, t := range c.Tables {
		sb.WriteString(fmt.Sprintf("let %s = [\n", name))
		for _, row := range t.Rows {
			sb.WriteString("  {\n")
			for _, col := range t.Columns {
				sb.WriteString(fmt.Sprintf("    %s: %s,\n", col, formatValue(row[col])))
			}
			sb.WriteString("  },\n")
		}
		sb.WriteString("]\n\n")
	}

	stmt, _ := sqlparser.Parse(c.Query)
	sel := stmt.(*sqlparser.Select)
	tblExpr := sel.From[0].(*sqlparser.AliasedTableExpr)
	tblName := tblExpr.Expr.(sqlparser.TableName).Name.String()
	cond := ""
	if sel.Where != nil {
		if cmp, ok := sel.Where.Expr.(*sqlparser.ComparisonExpr); ok {
			left := cmp.Left.(*sqlparser.ColName).Name.String()
			right := formatValue(evalExpr(cmp.Right, nil, nil))
			op := cmp.Operator
			if op == "=" {
				op = "=="
			}
			cond = fmt.Sprintf("row.%s %s %s", left, op, right)
		}
	}
	sb.WriteString("let result = count(from row in " + tblName)
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
