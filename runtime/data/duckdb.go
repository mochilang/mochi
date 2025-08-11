//go:build cgo && !js && !wasm

package data

import (
	"database/sql"
	"fmt"
	_ "github.com/marcboeker/go-duckdb"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// tableInfo tracks a temporary DuckDB table created from a scan source.
type tableInfo struct {
	name    string
	alias   string
	columns []string
}

// ExecPlanDuckDB executes the logical plan by translating it to SQL and running it in DuckDB.
// For plans containing grouping, it falls back to the in-memory execution.
func ExecPlanDuckDB(plan Plan, env *types.Env, eval func(*parser.Expr) (any, error)) ([]any, error) {
	sp, ok := plan.(*selectPlan)
	if !ok {
		return ExecPlan(plan, env, eval)
	}
	if containsGroup(sp.Input) {
		// grouping not yet supported in SQL translation
		return ExecPlan(plan, env, eval)
	}

	scans := gatherScans(sp)
	db, err := sql.Open("duckdb", "")
	if err != nil {
		return nil, err
	}
	defer db.Close()

	tables := map[*scanPlan]*tableInfo{}
	for idx, sc := range scans {
		tbl := fmt.Sprintf("t%d", idx)
		rows, colsTypes, err := materializeSource(env, eval, sc.Src)
		if err != nil {
			return nil, err
		}
		if len(colsTypes) == 0 {
			continue
		}
		cols := make([]string, 0, len(colsTypes))
		colDefs := make([]string, 0, len(colsTypes))
		for c, typ := range colsTypes {
			cols = append(cols, c)
			colDefs = append(colDefs, fmt.Sprintf("%s %s", c, typ))
		}
		if _, err := db.Exec(fmt.Sprintf("CREATE TABLE %s (%s)", tbl, strings.Join(colDefs, ","))); err != nil {
			return nil, err
		}
		for _, row := range rows {
			vals := make([]any, len(cols))
			qs := make([]string, len(cols))
			for i, c := range cols {
				vals[i] = row[c]
				qs[i] = "?"
			}
			if _, err := db.Exec(fmt.Sprintf("INSERT INTO %s (%s) VALUES (%s)", tbl, strings.Join(cols, ","), strings.Join(qs, ",")), vals...); err != nil {
				return nil, err
			}
		}
		tables[sc] = &tableInfo{name: tbl, alias: sc.Alias, columns: cols}
	}

	sqlPlan, err := buildSQL(sp.Input, tables, eval)
	if err != nil {
		return nil, err
	}

	rows, err := db.Query(sqlPlan.query)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	colNames, err := rows.Columns()
	if err != nil {
		return nil, err
	}

	results := []any{}
	for rows.Next() {
		vals := make([]any, len(colNames))
		ptrs := make([]any, len(colNames))
		for i := range vals {
			ptrs[i] = &vals[i]
		}
		if err := rows.Scan(ptrs...); err != nil {
			return nil, err
		}
		aliasObjs := map[string]map[string]any{}
		for i, col := range colNames {
			parts := strings.SplitN(col, ".", 2)
			if len(parts) != 2 {
				continue
			}
			a, f := parts[0], parts[1]
			m, ok := aliasObjs[a]
			if !ok {
				m = map[string]any{}
				aliasObjs[a] = m
			}
			m[f] = vals[i]
		}
		for alias, obj := range aliasObjs {
			env.SetValue(alias, obj, true)
		}
		val, err := eval(sp.Expr)
		if err != nil {
			return nil, err
		}
		results = append(results, val)
	}
	return results, rows.Err()
}

// materializeSource evaluates the scan source and returns rows and column names.
func materializeSource(env *types.Env, eval func(*parser.Expr) (any, error), src *parser.Expr) ([]map[string]any, map[string]string, error) {
	val, err := eval(src)
	if err != nil {
		return nil, nil, err
	}
	var list []any
	switch v := val.(type) {
	case []any:
		list = v
	case *Group:
		list = v.Items
	default:
		return nil, nil, fmt.Errorf("query source must be list, got %T", val)
	}
	colsMap := map[string]string{}
	rows := make([]map[string]any, 0, len(list))
	for _, it := range list {
		m, ok := it.(map[string]any)
		if !ok {
			return nil, nil, fmt.Errorf("row must be map, got %T", it)
		}
		rows = append(rows, m)
		for k, v := range m {
			typ := detectType(v)
			if exist, ok := colsMap[k]; ok {
				colsMap[k] = mergeType(exist, typ)
			} else {
				colsMap[k] = typ
			}
		}
	}
	return rows, colsMap, nil
}

// containsGroup reports whether the plan tree has a groupPlan node.
func containsGroup(pl Plan) bool {
	switch p := pl.(type) {
	case *groupPlan:
		return true
	case *selectPlan:
		return containsGroup(p.Input)
	case *wherePlan:
		return containsGroup(p.Input)
	case *sortPlan:
		return containsGroup(p.Input)
	case *limitPlan:
		return containsGroup(p.Input)
	case *joinPlan:
		return containsGroup(p.Left) || containsGroup(p.Right)
	default:
		return false
	}
}

type sqlPlan struct {
	query   string
	aliases map[string][]string
}

func mergeAliasMaps(a, b map[string][]string) map[string][]string {
	out := map[string][]string{}
	for k, v := range a {
		out[k] = v
	}
	for k, v := range b {
		out[k] = v
	}
	return out
}

func buildSQL(pl Plan, tables map[*scanPlan]*tableInfo, eval func(*parser.Expr) (any, error)) (sqlPlan, error) {
	switch p := pl.(type) {
	case *scanPlan:
		info := tables[p]
		cols := make([]string, len(info.columns))
		for i, c := range info.columns {
			cols[i] = fmt.Sprintf("%s AS \"%s.%s\"", c, p.Alias, c)
		}
		q := fmt.Sprintf("SELECT %s FROM %s", strings.Join(cols, ","), info.name)
		return sqlPlan{q, map[string][]string{p.Alias: info.columns}}, nil
	case *wherePlan:
		child, err := buildSQL(p.Input, tables, eval)
		if err != nil {
			return sqlPlan{}, err
		}
		cond, err := exprToSQL(p.Cond, func(a string) (string, bool) {
			_, ok := child.aliases[a]
			return "", ok
		})
		if err != nil {
			return sqlPlan{}, err
		}
		q := fmt.Sprintf("SELECT * FROM (%s) WHERE %s", child.query, cond)
		return sqlPlan{q, child.aliases}, nil
	case *sortPlan:
		child, err := buildSQL(p.Input, tables, eval)
		if err != nil {
			return sqlPlan{}, err
		}
		key, err := exprToSQL(p.Key, func(a string) (string, bool) {
			_, ok := child.aliases[a]
			return "", ok
		})
		if err != nil {
			return sqlPlan{}, err
		}
		q := fmt.Sprintf("SELECT * FROM (%s) ORDER BY %s", child.query, key)
		return sqlPlan{q, child.aliases}, nil
	case *limitPlan:
		child, err := buildSQL(p.Input, tables, eval)
		if err != nil {
			return sqlPlan{}, err
		}
		q := fmt.Sprintf("SELECT * FROM (%s)", child.query)
		if p.Take != nil {
			v, err := eval(p.Take)
			if err != nil {
				return sqlPlan{}, err
			}
			if n, ok := v.(int); ok {
				q += fmt.Sprintf(" LIMIT %d", n)
			}
		}
		if p.Skip != nil {
			v, err := eval(p.Skip)
			if err != nil {
				return sqlPlan{}, err
			}
			if n, ok := v.(int); ok {
				q += fmt.Sprintf(" OFFSET %d", n)
			}
		}
		return sqlPlan{q, child.aliases}, nil
	case *joinPlan:
		left, err := buildSQL(p.Left, tables, eval)
		if err != nil {
			return sqlPlan{}, err
		}
		right, err := buildSQL(p.Right, tables, eval)
		if err != nil {
			return sqlPlan{}, err
		}
		cond := "TRUE"
		if p.On != nil {
			cond, err = exprToSQL(p.On, func(alias string) (string, bool) {
				if _, ok := left.aliases[alias]; ok {
					return "l.", true
				}
				if _, ok := right.aliases[alias]; ok {
					return "r.", true
				}
				return "", false
			})
			if err != nil {
				return sqlPlan{}, err
			}
		}
		q := fmt.Sprintf("SELECT l.*, r.* FROM (%s) AS l %s JOIN (%s) AS r ON %s", left.query, strings.ToUpper(p.JoinType), right.query, cond)
		return sqlPlan{q, mergeAliasMaps(left.aliases, right.aliases)}, nil
	default:
		return sqlPlan{}, fmt.Errorf("unsupported plan node %T", p)
	}
}

func exprToSQL(e *parser.Expr, resolve func(string) (string, bool)) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expression")
	}
	out, err := unaryToSQL(e.Binary.Left, resolve)
	if err != nil {
		return "", err
	}
	for _, op := range e.Binary.Right {
		// op.Right is a *parser.Unary, which may contain prefix operators
		// and a postfix expression.  The previous implementation attempted
		// to pass it directly to postfixToSQL which expects a *parser.PostfixExpr
		// and resulted in a compile-time type mismatch after parser refactors.
		// Use unaryToSQL so both prefix operators and the inner postfix
		// expression are handled correctly.
		right, err := unaryToSQL(op.Right, resolve)
		if err != nil {
			return "", err
		}
		oper := op.Op
		switch oper {
		case "==":
			oper = "="
		case "&&":
			oper = "AND"
		case "||":
			oper = "OR"
		}
		out = fmt.Sprintf("(%s %s %s)", out, oper, right)
	}
	return out, nil
}

func unaryToSQL(u *parser.Unary, resolve func(string) (string, bool)) (string, error) {
	val, err := postfixToSQL(u.Value, resolve)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = "-" + val
		case "!":
			val = "NOT " + val
		}
	}
	return val, nil
}

func postfixToSQL(p *parser.PostfixExpr, resolve func(string) (string, bool)) (string, error) {
	return primaryToSQL(p.Target, resolve)
}

func primaryToSQL(p *parser.Primary, resolve func(string) (string, bool)) (string, error) {
	switch {
	case p.Lit != nil:
		return literalToSQL(p.Lit), nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return "", fmt.Errorf("bare identifier %s", p.Selector.Root)
		}
		if prefix, ok := resolve(p.Selector.Root); ok {
			return fmt.Sprintf("%s\"%s.%s\"", prefix, p.Selector.Root, strings.Join(p.Selector.Tail, ".")), nil
		}
		return "", fmt.Errorf("unknown identifier %s", p.Selector.Root)
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func literalToSQL(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float)
	case l.Str != nil:
		s := strings.ReplaceAll(*l.Str, "'", "''")
		return fmt.Sprintf("'%s'", s)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "TRUE"
		}
		return "FALSE"
	default:
		return "NULL"
	}
}

func gatherScans(plan Plan) []*scanPlan {
	var scans []*scanPlan
	switch p := plan.(type) {
	case *selectPlan:
		scans = append(scans, gatherScans(p.Input)...)
	case *wherePlan:
		scans = append(scans, gatherScans(p.Input)...)
	case *sortPlan:
		scans = append(scans, gatherScans(p.Input)...)
	case *limitPlan:
		scans = append(scans, gatherScans(p.Input)...)
	case *joinPlan:
		scans = append(scans, gatherScans(p.Left)...)
		scans = append(scans, gatherScans(p.Right)...)
	case *scanPlan:
		scans = append(scans, p)
	}
	return scans
}

func detectType(v any) string {
	switch v.(type) {
	case int, int64:
		return "INTEGER"
	case float64:
		return "DOUBLE"
	case bool:
		return "BOOLEAN"
	default:
		return "TEXT"
	}
}

func mergeType(a, b string) string {
	if a == b {
		return a
	}
	if a == "TEXT" || b == "TEXT" {
		return "TEXT"
	}
	if a == "DOUBLE" || b == "DOUBLE" {
		return "DOUBLE"
	}
	if a == "INTEGER" && b == "BOOLEAN" || a == "BOOLEAN" && b == "INTEGER" {
		return "INTEGER"
	}
	return a
}
