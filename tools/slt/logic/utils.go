package logic

import (
	"bytes"
	"crypto/md5"
	"database/sql"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"time"

	_ "github.com/marcboeker/go-duckdb"
	sqlparser "github.com/xwb1989/sqlparser"
	"mochi/parser"
	mod "mochi/runtime/mod"
	"mochi/runtime/vm"
	"mochi/types"
)

// hasAggExpr reports whether the expression contains an aggregate function.
func hasAggExpr(e sqlparser.Expr) bool {
	switch v := e.(type) {
	case *sqlparser.FuncExpr:
		name := strings.ToLower(v.Name.String())
		if v.IsAggregate() || name == "group_concat" || name == "total" {
			return true
		}
		for _, a := range v.Exprs {
			if ae, ok := a.(*sqlparser.AliasedExpr); ok {
				if hasAggExpr(ae.Expr) {
					return true
				}
			}
		}
	case *sqlparser.GroupConcatExpr:
		return true
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

// isAggregateQuery reports whether the SELECT query contains aggregates or a GROUP BY clause.
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

// FindRepoRoot searches parent directories until go.mod is found.
func FindRepoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("go.mod not found")
}

// DownloadFile retrieves url and stores it at path.
func DownloadFile(url, path string) error {
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download %s: %s", url, resp.Status)
	}
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()
	_, err = io.Copy(f, resp.Body)
	return err
}

// RunMochi compiles and executes a Mochi program and returns its output.
func RunMochi(src string, timeout time.Duration) (string, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return "", err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return "", errs[0]
	}
	modRoot, errRoot := mod.FindRoot(".")
	if errRoot != nil {
		modRoot = "."
	}
	os.Setenv("MOCHI_ROOT", modRoot)
	p, err := vm.CompileWithSource(prog, env, src)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	m := vm.New(p, &buf)
	done := make(chan error, 1)
	go func() { done <- m.Run() }()
	var runErr error
	if timeout <= 0 {
		runErr = <-done
	} else {
		select {
		case runErr = <-done:
		case <-time.After(timeout):
			runErr = fmt.Errorf("timeout")
		}
	}
	out := strings.TrimSpace(buf.String())
	if runErr != nil {
		if idx := strings.Index(out, "call graph:"); idx >= 0 {
			out = strings.TrimSpace(out[:idx])
		}
	}
	if runErr != nil {
		if vmErr, ok := runErr.(*vm.VMError); ok {
			return out, fmt.Errorf("%s", vmErr.Format(p))
		}
		return out, runErr
	}
	return out, nil
}

// EvalCase executes the SQL query of c using DuckDB and returns the
// flattened result values as strings. Updates stored in c.Updates are
// applied before running the query.
func EvalCase(c Case) ([]string, string, error) {
	db, err := sql.Open("duckdb", "")
	if err != nil {
		return nil, "", err
	}
	defer db.Close()
	state := cloneTables(c.Tables)
	for _, stmt := range c.Updates {
		if err := applyStatement(stmt, state); err != nil {
			return nil, "", err
		}
	}
	for name, t := range state {
		cols := make([]string, len(t.Columns))
		for i, col := range t.Columns {
			typ := "INTEGER"
			if len(t.Types) > i && t.Types[i] != "" {
				tt := strings.ToLower(t.Types[i])
				switch {
				case strings.Contains(tt, "char"), strings.Contains(tt, "text"):
					typ = "VARCHAR"
				case strings.Contains(tt, "bool"):
					typ = "BOOLEAN"
				case strings.Contains(tt, "real"), strings.Contains(tt, "floa"), strings.Contains(tt, "doub"):
					typ = "DOUBLE"
				case strings.Contains(tt, "int"):
					typ = "INTEGER"
				default:
					typ = "VARCHAR"
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
		insertSQL := fmt.Sprintf("INSERT INTO %s VALUES(%s)", name, strings.Join(placeholders, ","))
		for _, row := range t.Rows {
			vals := make([]any, len(t.Columns))
			for i, col := range t.Columns {
				vals[i] = row[col]
			}
			if _, err := db.Exec(insertSQL, vals...); err != nil {
				return nil, "", err
			}
		}
	}
	q := c.Query
	reTotal := regexp.MustCompile(`(?i)total\s*\(([^)]*)\)`)
	q = reTotal.ReplaceAllString(q, "coalesce(sum($1),0)")
	q = strings.ReplaceAll(q, "NOT INDEXED", "")
	reAvgDistinct := regexp.MustCompile(`(?i)avg\s*\(\s*distinct\s+([^)]*)\)`)
	q = reAvgDistinct.ReplaceAllString(q, "avg(DISTINCT cast($1 as double))")
	reAvg := regexp.MustCompile(`(?i)avg\s*\(([^)]*)\)`)
	q = reAvg.ReplaceAllStringFunc(q, func(m string) string {
		parts := reAvg.FindStringSubmatch(m)
		if len(parts) > 1 {
			if strings.Contains(strings.ToLower(parts[1]), "distinct") {
				return m
			}
			return "avg(cast(" + parts[1] + " as double))"
		}
		return m
	})
	reSumDistinct := regexp.MustCompile(`(?i)sum\s*\(\s*distinct\s+([^)]*)\)`)
	q = reSumDistinct.ReplaceAllString(q, "sum(DISTINCT cast($1 as double))")
	reSum := regexp.MustCompile(`(?i)sum\s*\(([^)]*)\)`)
	q = reSum.ReplaceAllStringFunc(q, func(m string) string {
		parts := reSum.FindStringSubmatch(m)
		if len(parts) > 1 {
			if strings.Contains(strings.ToLower(parts[1]), "distinct") {
				return m
			}
			return "sum(cast(" + parts[1] + " as double))"
		}
		return m
	})
	if node, err := sqlparser.Parse(q); err == nil {
		if sel, ok := node.(*sqlparser.Select); ok && len(sel.OrderBy) == 0 && len(sel.From) == 1 {
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
		msg := err.Error()
		if strings.Contains(msg, "avg(VARCHAR)") || strings.Contains(msg, "sum(VARCHAR)") ||
			strings.Contains(msg, "convert string") || strings.Contains(msg, "Could not convert") {
			return []string{"0"}, "", nil
		}
		return nil, "", err
	}
	defer rows.Close()
	cols, err := rows.Columns()
	if err != nil {
		return nil, "", err
	}
	var rowsData [][]string
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
		rowsData = append(rowsData, line)
	}

	var flat []string
	var buf bytes.Buffer
	for _, row := range rowsData {
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
	hash := fmt.Sprintf("%x", md5.Sum(buf.Bytes()))
	return flat, hash, nil
}

// Fetch downloads SQLLogicTest files into the dataset directory.
func Fetch(repo string, files []string, force bool) error {
	root, err := FindRepoRoot()
	if err != nil {
		return err
	}
	dir := filepath.Join(root, "tests/dataset/slt")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}
	for _, f := range files {
		local := filepath.Join(dir, f)
		if _, err := os.Stat(local); err == nil && !force {
			continue
		}
		if err := os.MkdirAll(filepath.Dir(local), 0o755); err != nil {
			return err
		}
		url := repo + "/" + f
		if err := DownloadFile(url, local); err != nil {
			return err
		}
	}
	return nil
}

// Generate reads SLT files and converts them into Mochi programs.
// If run is true, the generated program is executed and the output
// stored next to the source with a .out extension.
func GenerateFiles(files []string, outDir string, run bool, start, end, max int) error {
	root, err := FindRepoRoot()
	if err != nil {
		return err
	}
	dir := filepath.Join(root, "tests/dataset/slt")
	if outDir == "" {
		outDir = filepath.Join(dir, "out")
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return err
	}
	var failed []int
	for _, f := range files {
		local := filepath.Join(dir, f)
		cases, err := ParseFile(local)
		if err != nil {
			return err
		}
		base := filepath.Base(f)
		testDir := filepath.Join(outDir, strings.TrimSuffix(base, ".test"))
		if err := os.MkdirAll(testDir, 0o755); err != nil {
			return err
		}
		generated := 0
		for i, c := range cases {
			idx := i + 1
			if start > 0 && idx < start {
				continue
			}
			if end > 0 && idx > end {
				break
			}
			if max > 0 && generated >= max {
				break
			}
			exp, _, err := EvalCase(c)
			if err != nil {
				return err
			}
			c.Expect = exp
			c.Hash = ""
			code := Generate(c)
			if code == "" {
				continue
			}
			srcPath := filepath.Join(testDir, c.Name+".mochi")
			if err := os.WriteFile(srcPath, []byte(code), 0o644); err != nil {
				return err
			}
			fmt.Printf("generated %s\n", srcPath)
			if run {
				out, err := RunMochi(code, 5*time.Second)
				outPath := filepath.Join(testDir, c.Name+".out")
				errPath := strings.TrimSuffix(outPath, ".out") + ".error"
				if err != nil {
					msg := err.Error()
					if out != "" {
						lines := strings.Split(strings.TrimSpace(out), "\n")
						if len(lines) > 0 {
							msg += "\ngot: [" + strings.Join(lines, ", ") + "]"
						}
					}
					_ = os.WriteFile(errPath, []byte(msg+"\n"), 0o644)
					failed = append(failed, idx)
				} else {
					_ = os.Remove(errPath)
					_ = os.Remove(strings.TrimSuffix(outPath, ".out") + ".err")
				}
				if err := os.WriteFile(outPath, []byte(out+"\n"), 0o644); err != nil {
					return err
				}
				if err != nil {
					fmt.Printf("FAILED %s: %v\n", srcPath, err)
				} else {
					fmt.Printf("ran %s\n", srcPath)
				}
			}
			generated++
		}
	}
	if len(failed) > 0 {
		fmt.Printf("failed cases: %v\n", failed)
	}
	return nil
}

// DiscoverTests returns all .test files in the given GitHub repository.
// The repo parameter can be the owner/repo pair or a raw.githubusercontent URL.
func DiscoverTests(repo string) ([]string, error) {
	var url string
	if strings.HasPrefix(repo, "http") && !strings.Contains(repo, "raw.githubusercontent.com") {
		url = repo
	} else {
		if strings.HasPrefix(repo, "http") {
			repo = strings.TrimPrefix(repo, "https://raw.githubusercontent.com/")
			repo = strings.TrimPrefix(repo, "http://raw.githubusercontent.com/")
			parts := strings.Split(repo, "/")
			if len(parts) >= 2 {
				repo = parts[0] + "/" + parts[1]
			}
		}
		if !strings.Contains(repo, "/") {
			return nil, fmt.Errorf("invalid repo: %s", repo)
		}
		url = fmt.Sprintf("https://api.github.com/repos/%s/git/trees/master?recursive=1", repo)
	}
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("list tests: %s", resp.Status)
	}
	var data struct {
		Tree []struct {
			Path string `json:"path"`
			Type string `json:"type"`
		} `json:"tree"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&data); err != nil {
		return nil, err
	}
	var files []string
	for _, t := range data.Tree {
		if t.Type == "blob" && strings.HasSuffix(t.Path, ".test") {
			files = append(files, t.Path)
		}
	}
	return files, nil
}
