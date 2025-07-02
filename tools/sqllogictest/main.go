package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"database/sql"
	_ "github.com/marcboeker/go-duckdb"

	"mochi/parser"
	mod "mochi/runtime/mod"
	"mochi/runtime/vm"
	"mochi/types"
)

type TestCase struct {
	Name       string
	Statements []string
	Query      string
}

func findRepoRoot() (string, error) {
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

func ensureTests(dir string) error {
	if _, err := os.Stat(filepath.Join(dir, "test")); err == nil {
		return nil
	}
	fmt.Println("downloading sqllogictest...")
	tmp, err := os.MkdirTemp("", "sqllogictest")
	if err != nil {
		return err
	}
	defer os.RemoveAll(tmp)
	cmd := exec.Command("git", "clone", "--depth", "1", "https://github.com/cockroachdb/sqllogictest.git", tmp)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	return copyDir(filepath.Join(tmp, "test"), filepath.Join(dir, "test"))
}

func copyDir(src, dst string) error {
	return filepath.Walk(src, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(src, path)
		if err != nil {
			return err
		}
		target := filepath.Join(dst, rel)
		if info.IsDir() {
			return os.MkdirAll(target, 0755)
		}
		data, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		return os.WriteFile(target, data, 0644)
	})
}

func parseTestFile(path string) ([]TestCase, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	var statements []string
	var cases []TestCase
	idx := 0
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(line, "statement") {
			var sqlLines []string
			for scanner.Scan() {
				l := scanner.Text()
				if strings.TrimSpace(l) == "" {
					break
				}
				sqlLines = append(sqlLines, l)
			}
			statements = append(statements, strings.Join(sqlLines, "\n"))
		} else if strings.HasPrefix(line, "query") {
			var qLines []string
			for scanner.Scan() {
				l := scanner.Text()
				if strings.HasPrefix(l, "----") {
					break
				}
				qLines = append(qLines, l)
			}
			// skip expected section
			for scanner.Scan() {
				l := strings.TrimSpace(scanner.Text())
				if l == "" {
					break
				}
			}
			cases = append(cases, TestCase{
				Name:       fmt.Sprintf("%s_%d", strings.TrimSuffix(filepath.Base(path), filepath.Ext(path)), idx),
				Statements: append([]string(nil), statements...),
				Query:      strings.Join(qLines, "\n"),
			})
			idx++
		}
	}
	return cases, scanner.Err()
}

func runCase(tc TestCase) (string, error) {
	db, err := sql.Open("duckdb", "")
	if err != nil {
		return "", err
	}
	defer db.Close()
	for _, st := range tc.Statements {
		if _, err := db.Exec(st); err != nil {
			return "", err
		}
	}
	rows, err := db.Query(tc.Query)
	if err != nil {
		return "", err
	}
	defer rows.Close()
	cols, err := rows.Columns()
	if err != nil {
		return "", err
	}
	var out []map[string]any
	for rows.Next() {
		vals := make([]any, len(cols))
		ptrs := make([]any, len(cols))
		for i := range vals {
			ptrs[i] = &vals[i]
		}
		if err := rows.Scan(ptrs...); err != nil {
			return "", err
		}
		m := make(map[string]any, len(cols))
		for i, c := range cols {
			if b, ok := vals[i].([]byte); ok {
				m[c] = string(b)
			} else {
				m[c] = vals[i]
			}
		}
		out = append(out, m)
	}
	data, err := json.Marshal(out)
	if err != nil {
		return "", err
	}
	return string(data), rows.Err()
}

func genMochi(tc TestCase, out string) ([]byte, error) {
	var b bytes.Buffer
	fmt.Fprintln(&b, "import sql")
	fmt.Fprintln(&b, "import json")
	fmt.Fprintln(&b)
	fmt.Fprintln(&b, "let db = sql.open(\"duckdb\", \"\")")
	fmt.Fprintln(&b)
	for _, st := range tc.Statements {
		fmt.Fprintf(&b, "db.exec(`%s`)\n", strings.ReplaceAll(st, "`", "`+"+"`"+"`"))
	}
	fmt.Fprintln(&b)
	fmt.Fprintf(&b, "let result = db.query(`%s`)\n", strings.ReplaceAll(tc.Query, "`", "`+"+"`"+"`"))
	fmt.Fprintln(&b, "json(result)")
	return b.Bytes(), nil
}

func compileAndRun(srcPath, outPath string) error {
	prog, err := parser.Parse(srcPath)
	if err != nil {
		return err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return errs[0]
	}
	modRoot, errRoot := mod.FindRoot(filepath.Dir(srcPath))
	if errRoot != nil {
		modRoot = filepath.Dir(srcPath)
	}
	os.Setenv("MOCHI_ROOT", modRoot)
	p, err := vm.Compile(prog, env)
	if err != nil {
		return err
	}
	var buf bytes.Buffer
	m := vm.New(p, &buf)
	if err := m.Run(); err != nil {
		return err
	}
	return os.WriteFile(outPath, []byte(strings.TrimSpace(buf.String())), 0644)
}

func main() {
	root, err := findRepoRoot()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	dir := filepath.Join(root, "tests/dataset/logic")
	if err := ensureTests(dir); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	var testFiles []string
	filepath.Walk(filepath.Join(dir, "test"), func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if info.IsDir() {
			return nil
		}
		if strings.HasSuffix(path, ".test") {
			testFiles = append(testFiles, path)
		}
		return nil
	})
	outDir := filepath.Join(dir, "out")
	os.MkdirAll(outDir, 0755)
	for _, tf := range testFiles {
		cases, err := parseTestFile(tf)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", tf, err)
			continue
		}
		for _, c := range cases {
			code, _ := genMochi(c, outDir)
			srcPath := filepath.Join(dir, c.Name+".mochi")
			os.WriteFile(srcPath, code, 0644)
			result, err := runCase(c)
			if err != nil {
				fmt.Fprintf(os.Stderr, "%s: run query: %v\n", c.Name, err)
				continue
			}
			outPath := filepath.Join(outDir, c.Name+".out")
			if err := os.WriteFile(outPath, []byte(result), 0644); err != nil {
				fmt.Fprintf(os.Stderr, "%s: write out: %v\n", c.Name, err)
				continue
			}
			if err := compileAndRun(srcPath, outPath); err != nil {
				fmt.Fprintf(os.Stderr, "%s: run mochi: %v\n", c.Name, err)
			}
		}
	}
}
