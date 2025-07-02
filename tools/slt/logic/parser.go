package logic

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"

	sqlparser "github.com/xwb1989/sqlparser"
)

// Table represents an in-memory table with ordered columns.
type Table struct {
	Columns []string
	Rows    []map[string]any
}

// Case describes a single query together with the data state at that point.
// Case describes a single query together with the data state at that point.
// Comments contains the literal comments and SQL statements that appeared in
// the test file leading up to the query. They are reproduced in the generated
// Mochi program as comments so that the origin of each case is visible.
type Case struct {
	Name     string
	Tables   map[string]*Table
	Query    string
	Expect   []string
	Comments []string
}

// ParseFile parses a sqllogictest script and returns a Case for each query.
func ParseFile(path string) ([]Case, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	tables := make(map[string]*Table)
	var cases []Case
	count := 0
	// comments collects both '#' lines and SQL statements until the next
	// query, preserving their original order so they can be emitted as
	// comments in the generated Mochi source.
	var comments []string

	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		switch {
		case strings.HasPrefix(line, "#"):
			c := strings.TrimSpace(strings.TrimPrefix(line, "#"))
			comments = append(comments, c)
		case strings.HasPrefix(line, "statement ok"):
			if !scanner.Scan() {
				break
			}
			stmt := strings.TrimSpace(scanner.Text())
			comments = append(comments, "SQL: "+stmt)
			if err := applyStatement(stmt, tables); err != nil {
				return nil, err
			}
		case strings.HasPrefix(line, "statement error"):
			if !scanner.Scan() {
				break
			}
			stmt := strings.TrimSpace(scanner.Text())
			comments = append(comments, "SQL: "+stmt)
		case strings.HasPrefix(line, "query"):
			if !scanner.Scan() {
				break
			}
			q := strings.TrimSpace(scanner.Text())
			// skip until ----
			for scanner.Scan() {
				if strings.TrimSpace(scanner.Text()) == "----" {
					break
				}
			}
			var expect []string
			for scanner.Scan() {
				l := strings.TrimSpace(scanner.Text())
				if l == "" {
					break
				}
				expect = append(expect, l)
			}
			count++
			cases = append(cases, Case{
				Name:     fmt.Sprintf("case%d", count),
				Tables:   cloneTables(tables),
				Query:    q,
				Expect:   expect,
				Comments: comments,
			})
			comments = nil
		}
	}
	return cases, scanner.Err()
}

func cloneTables(src map[string]*Table) map[string]*Table {
	dst := make(map[string]*Table)
	for name, t := range src {
		nt := &Table{Columns: append([]string(nil), t.Columns...)}
		for _, row := range t.Rows {
			nr := make(map[string]any)
			for k, v := range row {
				nr[k] = v
			}
			nt.Rows = append(nt.Rows, nr)
		}
		dst[name] = nt
	}
	return dst
}

func applyStatement(stmt string, tables map[string]*Table) error {
	node, err := sqlparser.Parse(stmt)
	if err != nil {
		return err
	}
	switch s := node.(type) {
	case *sqlparser.DDL:
		if strings.EqualFold(s.Action, "create") && s.TableSpec != nil {
			cols := make([]string, len(s.TableSpec.Columns))
			for i, c := range s.TableSpec.Columns {
				cols[i] = c.Name.String()
			}
			tables[s.NewName.Name.String()] = &Table{Columns: cols}
		}
	case *sqlparser.Insert:
		tbl := s.Table.Name.String()
		t := tables[tbl]
		if t == nil {
			return fmt.Errorf("unknown table %s", tbl)
		}
		vals := s.Rows.(sqlparser.Values)
		for _, tuple := range vals {
			row := make(map[string]any)
			for i, expr := range tuple {
				if i >= len(t.Columns) {
					continue
				}
				row[t.Columns[i]] = evalExpr(expr, nil, nil)
			}
			t.Rows = append(t.Rows, row)
		}
	case *sqlparser.Update:
		tblExpr := s.TableExprs[0].(*sqlparser.AliasedTableExpr)
		tblName := tblExpr.Expr.(sqlparser.TableName).Name.String()
		t := tables[tblName]
		if t == nil {
			return fmt.Errorf("unknown table %s", tblName)
		}
		for idx, row := range t.Rows {
			if matchWhere(row, s.Where) {
				for _, assign := range s.Exprs {
					row[assign.Name.Name.String()] = evalExpr(assign.Expr, row, t)
				}
				t.Rows[idx] = row
			}
		}
	}
	return nil
}

func evalExpr(expr sqlparser.Expr, row map[string]any, table *Table) any {
	switch v := expr.(type) {
	case *sqlparser.SQLVal:
		switch v.Type {
		case sqlparser.StrVal:
			return string(v.Val)
		case sqlparser.IntVal:
			n, _ := strconv.Atoi(string(v.Val))
			return n
		case sqlparser.FloatVal:
			f, _ := strconv.ParseFloat(string(v.Val), 64)
			return f
		}
	case *sqlparser.NullVal:
		return nil
	case *sqlparser.ColName:
		if row != nil {
			return row[v.Name.String()]
		}
	case *sqlparser.BinaryExpr:
		l := evalExpr(v.Left, row, table)
		r := evalExpr(v.Right, row, table)
		lf, lok := toFloat(l)
		rf, rok := toFloat(r)
		if lok && rok {
			switch v.Operator {
			case "+":
				res := lf + rf
				if isInt(l) && isInt(r) {
					return int(res)
				}
				return res
			case "-":
				res := lf - rf
				if isInt(l) && isInt(r) {
					return int(res)
				}
				return res
			case "*":
				res := lf * rf
				if isInt(l) && isInt(r) {
					return int(res)
				}
				return res
			case "/":
				if rf != 0 {
					return lf / rf
				}
			}
		}
	}
	return nil
}

func matchWhere(row map[string]any, where *sqlparser.Where) bool {
	if where == nil {
		return true
	}
	cmp, ok := where.Expr.(*sqlparser.ComparisonExpr)
	if !ok {
		return false
	}
	col := cmp.Left.(*sqlparser.ColName).Name.String()
	rv := evalExpr(cmp.Right, row, nil)
	lv := row[col]

	lf, lok := toFloat(lv)
	rf, rok := toFloat(rv)

	switch cmp.Operator {
	case "=", "==":
		return equalValues(lv, rv)
	case ">":
		if lok && rok {
			return lf > rf
		}
	case "<":
		if lok && rok {
			return lf < rf
		}
	case ">=":
		if lok && rok {
			return lf >= rf
		}
	case "<=":
		if lok && rok {
			return lf <= rf
		}
	}
	return false
}

func equalValues(a, b any) bool {
	switch av := a.(type) {
	case int:
		switch bv := b.(type) {
		case int:
			return av == bv
		case float64:
			return float64(av) == bv
		}
	case float64:
		switch bv := b.(type) {
		case int:
			return av == float64(bv)
		case float64:
			return av == bv
		}
	case string:
		if bs, ok := b.(string); ok {
			return av == bs
		}
	case nil:
		return b == nil
	}
	return false
}

func toFloat(v any) (float64, bool) {
	switch t := v.(type) {
	case int:
		return float64(t), true
	case float64:
		return t, true
	case string:
		f, err := strconv.ParseFloat(t, 64)
		if err != nil {
			return 0, false
		}
		return f, true
	default:
		return 0, false
	}
}

func isInt(v any) bool {
	switch v.(type) {
	case int:
		return true
	}
	return false
}
