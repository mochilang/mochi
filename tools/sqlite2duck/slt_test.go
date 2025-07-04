package sqlite2duck

import (
    "crypto/md5"
    "database/sql"
    "fmt"
    "path/filepath"
    "sort"
    "strings"
    "testing"

    _ "github.com/mattn/go-sqlite3"
    sqlparser "github.com/xwb1989/sqlparser"
    "mochi/tools/slt/logic"
)

func hasAggExpr(e sqlparser.Expr) bool {
    switch v := e.(type) {
    case *sqlparser.FuncExpr:
        if v.IsAggregate() {
            return true
        }
        for _, ex := range v.Exprs {
            if ae, ok := ex.(*sqlparser.AliasedExpr); ok {
                if hasAggExpr(ae.Expr) {
                    return true
                }
            }
        }
    case *sqlparser.BinaryExpr:
        return hasAggExpr(v.Left) || hasAggExpr(v.Right)
    case *sqlparser.UnaryExpr:
        return hasAggExpr(v.Expr)
    case *sqlparser.ParenExpr:
        return hasAggExpr(v.Expr)
    case *sqlparser.CaseExpr:
        if v.Expr != nil && hasAggExpr(v.Expr) {
            return true
        }
        for _, w := range v.Whens {
            if hasAggExpr(w.Cond) || hasAggExpr(w.Val) {
                return true
            }
        }
        if v.Else != nil && hasAggExpr(v.Else) {
            return true
        }
    }
    return false
}

func isAggregateQuery(sel *sqlparser.Select) bool {
    if len(sel.GroupBy) > 0 {
        return true
    }
    for _, expr := range sel.SelectExprs {
        if ae, ok := expr.(*sqlparser.AliasedExpr); ok {
            if hasAggExpr(ae.Expr) {
                return true
            }
        }
    }
    if sel.Having != nil && hasAggExpr(sel.Having.Expr) {
        return true
    }
    return false
}

func evalCase(driver, dsn string, c logic.Case) ([]string, string, error) {
    db, err := sql.Open(driver, dsn)
    if err != nil {
        return nil, "", err
    }
    defer db.Close()

    for name, t := range c.Tables {
        cols := make([]string, len(t.Columns))
        for i, col := range t.Columns {
            typ := "INTEGER"
            if len(t.Types) > i && t.Types[i] != "" {
                tt := strings.ToLower(t.Types[i])
                switch {
                case strings.Contains(tt, "char"), strings.Contains(tt, "text"):
                    typ = "TEXT"
                case strings.Contains(tt, "bool"):
                    typ = "BOOLEAN"
                case strings.Contains(tt, "real"), strings.Contains(tt, "floa"), strings.Contains(tt, "doub"):
                    typ = "REAL"
                case strings.Contains(tt, "int"):
                    typ = "INTEGER"
                }
            }
            cols[i] = fmt.Sprintf("%s %s", col, typ)
        }
        if _, err := db.Exec(fmt.Sprintf("CREATE TABLE %s(%s)", name, strings.Join(cols, ","))); err != nil {
            return nil, "", err
        }
        placeholders := make([]string, len(t.Columns))
        for i := range placeholders {
            placeholders[i] = "?"
        }
        ins := fmt.Sprintf("INSERT INTO %s VALUES(%s)", name, strings.Join(placeholders, ","))
        for _, row := range t.Rows {
            vals := make([]any, len(t.Columns))
            for i, col := range t.Columns {
                vals[i] = row[col]
            }
            if _, err := db.Exec(ins, vals...); err != nil {
                return nil, "", err
            }
        }
    }
    for _, stmt := range c.Updates {
        if _, err := db.Exec(stmt); err != nil {
            return nil, "", err
        }
    }

    q := c.Query
    if node, err := sqlparser.Parse(q); err == nil {
        if sel, ok := node.(*sqlparser.Select); ok && len(sel.OrderBy) == 0 && len(sel.From) > 0 {
            if !isAggregateQuery(sel) {
                if tbl, ok := sel.From[0].(*sqlparser.AliasedTableExpr); ok {
                    if name, ok := tbl.Expr.(sqlparser.TableName); ok {
                        if !strings.EqualFold(name.Name.String(), "dual") {
                            q = strings.TrimSpace(q) + " ORDER BY rowid"
                        }
                    }
                }
            }
        }
    }
    rows, err := db.Query(q)
    if err != nil {
        return nil, "", err
    }
    defer rows.Close()
    cols, err := rows.Columns()
    if err != nil {
        return nil, "", err
    }
    var matrix [][]string
    for rows.Next() {
        vals := make([]any, len(cols))
        ptrs := make([]any, len(cols))
        for i := range vals {
            ptrs[i] = &vals[i]
        }
        if err := rows.Scan(ptrs...); err != nil {
            return nil, "", err
        }
        line := make([]string, len(cols))
        for i, v := range vals {
            if v == nil {
                line[i] = "null"
            } else {
                line[i] = fmt.Sprint(v)
            }
        }
        matrix = append(matrix, line)
    }
    var flat []string
    var buf strings.Builder
    for _, row := range matrix {
        for i, s := range row {
            flat = append(flat, s)
            if i > 0 {
                buf.WriteByte(' ')
            }
            buf.WriteString(s)
        }
        buf.WriteByte('\n')
    }
    if c.RowSort {
        sort.SliceStable(flat, func(i, j int) bool {
            ai, aj := flat[i], flat[j]
            if ai == "null" && aj != "null" {
                return false
            }
            if ai != "null" && aj == "null" {
                return true
            }
            return ai < aj
        })
    }
    hash := fmt.Sprintf("%x", md5.Sum([]byte(buf.String())))
    return flat, hash, nil
}

func TestSLTConvert(t *testing.T) {
    root, err := logic.FindRepoRoot()
    if err != nil {
        t.Fatal(err)
    }
    files := []string{
        filepath.Join(root, "tests/dataset/slt/select1.test"),
        filepath.Join(root, "tests/dataset/slt/select2.test"),
        filepath.Join(root, "tests/dataset/slt/select3.test"),
    }
    for _, f := range files {
        cases, err := logic.ParseFile(f)
        if err != nil {
            t.Fatalf("parse %s: %v", f, err)
        }
        for i, c := range cases {
            if i >= 5 {
                break
            }
            t.Run(filepath.Base(f)+c.Name, func(t *testing.T) {
                sqliteRes, _, err := evalCase("sqlite3", ":memory:", c)
                if err != nil {
                    t.Fatalf("sqlite: %v", err)
                }
                dc := c
                dc.Query = Convert(c.Query)
                duckRes, _, err := evalCase("duckdb", "", dc)
                if err != nil {
                    t.Fatalf("duck: %v", err)
                }
                if strings.Join(sqliteRes, ",") != strings.Join(duckRes, ",") {
                    t.Fatalf("results differ\nsqlite: %v\nduck: %v", sqliteRes, duckRes)
                }
            })
        }
    }
}

